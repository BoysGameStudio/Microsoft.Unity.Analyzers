﻿/*--------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See LICENSE in the project root for license information.
 *-------------------------------------------------------------------------------------------*/

using System.Threading.Tasks;
using Xunit;

namespace Microsoft.Unity.Analyzers.Tests;

public class MessageSuppressorTests : BaseSuppressorVerifierTest<MessageSuppressor>
{
	[Fact]
	public async Task UnusedStaticMethodAndParametersSuppressed()
	{
		const string test = @"
using UnityEditor;

class Processor : AssetPostprocessor
{
    private static string OnGeneratedCSProject(string path, string content)
    {
        return null;
    }
}";

		var suppressors = new[] {
			ExpectSuppressor(MessageSuppressor.MethodRule).WithLocation(6, 27),
			ExpectSuppressor(MessageSuppressor.ParameterRule).WithLocation(6, 55),
			ExpectSuppressor(MessageSuppressor.ParameterRule).WithLocation(6, 68),
		};

		await VerifyCSharpDiagnosticAsync(test, suppressors);
	}

	[Fact]
	public async Task UnusedAwaitableStaticMethodAndParametersSuppressed()
	{
		const string test = @"
using UnityEngine;
using UnityEditor;

class Processor : AssetPostprocessor
{
    private async static Awaitable<string> OnGeneratedCSProject(string path, string content)
    {
        await Awaitable.WaitForSecondsAsync(1);
        return null;
    }
}";

		var suppressors = new[] {
			ExpectSuppressor(MessageSuppressor.MethodRule).WithLocation(7, 44),
			ExpectSuppressor(MessageSuppressor.ParameterRule).WithLocation(7, 72),
			ExpectSuppressor(MessageSuppressor.ParameterRule).WithLocation(7, 85),
		};

		await VerifyCSharpDiagnosticAsync(test, suppressors);
	}


	[Fact]
	public async Task UnusedMethodSuppressed()
	{
		const string test = @"
using UnityEngine;

public class TestScript : MonoBehaviour
{
    private void Start()
    {
    }
}
";

		var suppressor = ExpectSuppressor(MessageSuppressor.MethodRule)
			.WithLocation(6, 18);

		await VerifyCSharpDiagnosticAsync(test, suppressor);
	}

	[Fact]
	public async Task UnusedAwaitableMethodSuppressed()
	{
		const string test = @"
using UnityEngine;

public class TestScript : MonoBehaviour
{
    private async Awaitable Start()
    {
        await Awaitable.WaitForSecondsAsync(1);
    }
}
";

		var suppressor = ExpectSuppressor(MessageSuppressor.MethodRule)
			.WithLocation(6, 29);

		await VerifyCSharpDiagnosticAsync(test, suppressor);
	}


	[Fact]
	public async Task UnusedMethodWithCrefSuppressed()
	{
		const string test = @"
using UnityEngine;

public class TestScript : MonoBehaviour
{
    private void Start()
    {
    }

	/// <summary>
	/// IDE0052 should be suppressed <see cref=""Start"" />
	/// </summary>
	public float speed = 0f;
}
";

		var suppressor = ExpectSuppressor(MessageSuppressor.MethodCrefRule)
			.WithLocation(6, 18);

		await VerifyCSharpDiagnosticAsync(test, suppressor);
	}

	[Fact]
	public async Task UnusedParameterSuppressed()
	{
		const string test = @"
using UnityEngine;

public class TestScript : MonoBehaviour
{
    private void OnAnimatorIK(int layerIndex)
    {
        OnAnimatorIK(0);
    }
}
";

		var suppressor = ExpectSuppressor(MessageSuppressor.ParameterRule)
			.WithLocation(6, 35);

		await VerifyCSharpDiagnosticAsync(test, suppressor);
	}

	[Fact]
	public async Task UnusedMethodScriptedImporterSuppressed()
	{
		const string test = @"
using System;
using UnityEditor.AssetImporters;

internal class Test : ScriptedImporter
{
    private void Reset()
    {
    }

    private static string[] GatherDependenciesFromSourceFile(string assetPath)
    {
        return null;
    }

    private void OnValidate()
    {
    }

    public override void OnImportAsset(AssetImportContext ctx)
    {
    }

    public override bool SupportsRemappedAssetType(Type type)
    {
        return true;
    }
}
";

		var suppressors = new[] {
			ExpectSuppressor(MessageSuppressor.MethodRule).WithLocation(7, 18),
			ExpectSuppressor(MessageSuppressor.MethodRule).WithLocation(11, 29),
			ExpectSuppressor(MessageSuppressor.ParameterRule).WithLocation(11, 69),
			ExpectSuppressor(MessageSuppressor.MethodRule).WithLocation(16, 18),
		};

		await VerifyCSharpDiagnosticAsync(test, suppressors);
	}
}
