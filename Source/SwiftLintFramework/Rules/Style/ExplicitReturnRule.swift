import Foundation
import SourceKittenFramework
import SwiftSyntax

public struct ExplicitReturnRule: ConfigurationProviderRule, SubstitutionCorrectableRule, OptInRule {
    public var configuration = ExplicitReturnConfiguration()

    public init() {}

    public static let description = RuleDescription(
        identifier: "explicit_return",
        name: "Explicit Return",
        description: "Prefer explicit returns in closures, functions and getters.",
        kind: .style,
        nonTriggeringExamples: ExplicitReturnRuleExamples.nonTriggeringExamples,
        triggeringExamples: ExplicitReturnRuleExamples.triggeringExamples,
        corrections: ExplicitReturnRuleExamples.corrections
    )

    public func validate(file: SwiftLintFile) -> [StyleViolation] {
        return violationOffsets(file: file).map {
            StyleViolation(
                ruleDescription: Self.description,
                severity: configuration.severityConfiguration.severity,
                location: Location(file: file, byteOffset: $0)
            )
        }
    }

    public func violationRanges(in file: SwiftLintFile) -> [NSRange] {
        return violationOffsets(file: file).compactMap {
            file.stringView.byteRangeToNSRange(ByteRange(location: $0, length: 0))
        }
    }

    public func substitution(for violationRange: NSRange, in file: SwiftLintFile) -> (NSRange, String)? {
        return (violationRange, "return ")
    }

    private func violationOffsets(file: SwiftLintFile) -> [ByteCount] {
        guard let tree = file.syntaxTree else { return [] }

        let visitors: [ViolationReporting] = [
            ClosureExprSyntaxVisitor(),
            FunctionDeclSyntaxVisitor(),
            VariableDeclSyntaxVisitor()
        ].filter {
            configuration.isKindIncluded($0.kind)
        }

        for visitor in visitors {
            visitor.walk(tree)
        }

        return visitors
            .reduce(into: [], { $0.append(contentsOf: $1.positions) })
            .map { ByteCount($0.utf8Offset) }
    }
}

private protocol ViolationReporting: SyntaxVisitor {
    var kind: ExplicitReturnConfiguration.ReturnKind { get }
    var positions: [AbsolutePosition] { get }
}

private final class ClosureExprSyntaxVisitor: SyntaxVisitor, ViolationReporting {
    let kind: ExplicitReturnConfiguration.ReturnKind = .closure

    private(set) var positions: [AbsolutePosition] = []

    override func visitPost(_ node: ClosureExprSyntax) {
        guard let firstItem = node.statements.first?.item,
              node.statements.count == 1 else { return }

        if firstItem.isImplicitlyReturnable {
            positions.append(firstItem.positionAfterSkippingLeadingTrivia)
        }
    }
}

private final class FunctionDeclSyntaxVisitor: SyntaxVisitor, ViolationReporting {
    let kind: ExplicitReturnConfiguration.ReturnKind = .function

    private(set) var positions: [AbsolutePosition] = []

    override func visitPost(_ node: FunctionDeclSyntax) {
        guard node.signature.allowsImplicitReturns,
              let firstItem = node.body?.statements.first?.item,
              node.body?.statements.count == 1 else { return }

        if firstItem.isImplicitlyReturnable {
            positions.append(firstItem.positionAfterSkippingLeadingTrivia)
        }
    }
}

private final class VariableDeclSyntaxVisitor: SyntaxVisitor, ViolationReporting {
    let kind: ExplicitReturnConfiguration.ReturnKind = .getter

    private(set) var positions: [AbsolutePosition] = []

    override func visitPost(_ node: VariableDeclSyntax) {
        for binding in node.bindings {
            if let accessor = binding.accessor?.as(CodeBlockSyntax.self) {
                // Shorthand syntax for getters: `var foo: Int { 0 }`
                guard let firstItem = accessor.statements.first?.item,
                      accessor.statements.count == 1 else { continue }

                if firstItem.isImplicitlyReturnable {
                    positions.append(firstItem.positionAfterSkippingLeadingTrivia)
                }
            } else if let accessorBlock = binding.accessor?.as(AccessorBlockSyntax.self) {
                // Full syntax for getters: `var foo: Int { get { 0 } }`
                guard let accessor = accessorBlock.accessors.first(where: { $0.accessorKind.text == "get" }),
                      let firstItem = accessor.body?.statements.first?.item,
                      accessor.body?.statements.count == 1 else { continue }

                if firstItem.isImplicitlyReturnable {
                    positions.append(firstItem.positionAfterSkippingLeadingTrivia)
                }
            }
        }
    }
}

private extension Syntax {
    var isImplicitlyReturnable: Bool {
        isProtocol(ExprSyntaxProtocol.self)
    }
}

private extension FunctionSignatureSyntax {
    var allowsImplicitReturns: Bool {
        if let simpleType = output?.returnType.as(SimpleTypeIdentifierSyntax.self) {
            return simpleType.name.text != "Void" && simpleType.name.text != "Never"
        } else if let tupleType = output?.returnType.as(TupleTypeSyntax.self) {
            return !tupleType.elements.isEmpty
        } else {
            return output != nil
        }
    }
}
