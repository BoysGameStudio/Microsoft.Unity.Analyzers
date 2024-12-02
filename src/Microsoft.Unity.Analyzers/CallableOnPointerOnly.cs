using System.Collections.Immutable;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace Microsoft.Unity.Analyzers;

[DiagnosticAnalyzer(LanguageNames.CSharp)]
public class CallableOnPointerOnlyAnalyzer : DiagnosticAnalyzer
{
	private const string RuleId = "UNT0038";
	private static readonly LocalizableString Title = "Do not call [CallableOnPointerOnly] methods directly";
	private static readonly LocalizableString MessageFormat = "Method '{0}' is marked with [CallableOnPointerOnly] and cannot be called directly";
	private static readonly LocalizableString Description = "Methods marked with [CallableOnPointerOnly] can only be called through pointers.";
	private const string Category = "Usage";

	internal static readonly DiagnosticDescriptor Rule = new(id: RuleId, Title, MessageFormat, Category, DiagnosticSeverity.Error, isEnabledByDefault: true, description: Description);

	public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create(Rule);

	public override void Initialize(AnalysisContext context)
	{
		context.EnableConcurrentExecution();
		context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.None);

		context.RegisterSyntaxNodeAction(AnalyzeInvocation, SyntaxKind.InvocationExpression);
	}

	private void AnalyzeInvocation(SyntaxNodeAnalysisContext context)
	{
		var invocationExpr = (InvocationExpressionSyntax)context.Node;

		// Get the symbol of the called method
		var symbolInfo = context.SemanticModel.GetSymbolInfo(invocationExpr);
		var methodSymbol = symbolInfo.Symbol as IMethodSymbol;

		if (methodSymbol == null)
			return;

		// Check if the method has the CallableOnPointerOnly attribute
		var hasRestrictedCallAttribute = methodSymbol.GetAttributes().Any(attr => attr.AttributeClass?.Name == "CallableOnPointerOnlyAttribute");

		if (!hasRestrictedCallAttribute)
		{
			return;
		}

		var containingType = methodSymbol.ContainingType;
		var currentSymbol = context.ContainingSymbol?.ContainingType;
		if (SymbolEqualityComparer.Default.Equals(containingType, currentSymbol))
		{
			return;
		}

		if (invocationExpr.Expression is MemberAccessExpressionSyntax memberAccessExpr && memberAccessExpr.OperatorToken.IsKind(SyntaxKind.MinusGreaterThanToken))
		{
			return;
		}

		// Report diagnostic
		var diagnostic = Diagnostic.Create(Rule, invocationExpr.GetLocation(), methodSymbol.Name);
		context.ReportDiagnostic(diagnostic);
	}
}
