/*
 * This file is part of CodeCadenza, a set of tools, libraries and plug-ins
 * for modeling and creating Java-based enterprise applications.
 * For more information visit:
 *
 * https://github.com/codecadenza/
 *
 * This software is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
 */
package net.codecadenza.eclipse.generator.testing.gui.imp;

import static net.codecadenza.eclipse.shared.Constants.PACK_PAGE_OBJECT;

import java.util.HashSet;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.testing.gui.action.GUITestActionGeneratorFactory;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.testing.GUITestAction;
import net.codecadenza.eclipse.model.testing.GUITestActionResultComponentType;
import net.codecadenza.eclipse.model.testing.GUITestActionType;
import net.codecadenza.eclipse.model.testing.GUITestCase;

/**
 * <p>
 * Generator for Selenium test cases
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SeleniumGUITestCaseGenerator extends AbstractJavaSourceGenerator {
	private final GUITestCase testCase;
	private final Project project;

	/**
	 * Constructor
	 * @param testCase
	 */
	public SeleniumGUITestCaseGenerator(GUITestCase testCase) {
		super(testCase.getSourceFile());

		this.testCase = testCase;
		this.project = testCase.getNamespace().getProject();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importPackage(testCase.getNamespace().toString() + PACK_PAGE_OBJECT);
		importPackage("net.codecadenza.runtime.selenium.junit");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("public class " + testCase.getName() + " extends AbstractSeleniumTestCase");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		final var b = new StringBuilder();
		final var methodSignature = "void test(SeleniumTestContext testContext)";

		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.selenium.junit.AbstractSeleniumTestCase#");
		b.append("test(net.codecadenza.runtime.selenium.junit.SeleniumTestContext)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");

		final var testActionDeclarationMap = new HashSet<String>();
		boolean isFirstAction = true;

		for (final GUITestAction testAction : testCase.getTestActions()) {
			if (testAction.needsTestData())
				importPackage("net.codecadenza.runtime.selenium.data");

			if (testAction.getActionResult() != null)
				if (project.hasJSFClient() || project.hasAngularClient()) {
					if (testAction.getActionResult().getComponentType() == GUITestActionResultComponentType.DIALOG) {
						if (project.hasJSFClient())
							importPackage("net.codecadenza.runtime.selenium.page.imp.primefaces");
						else if (project.hasAngularClient())
							importPackage("net.codecadenza.runtime.selenium.page.imp.angular");
					}
				}
				else
					importPackage("net.codecadenza.runtime.selenium.page.imp.vaadin");

			if (testAction.getType() == GUITestActionType.SEARCH_ROW_CURRENT_PAGE
					|| testAction.getType() == GUITestActionType.SEARCH_ROW_ALL_PAGES)
				importPackage("org.openqa.selenium");

			if (isFirstAction)
				isFirstAction = false;
			else
				b.append("\n");

			if (testAction.getDelayBefore() != null && testAction.getDelayBefore() >= 0) {
				b.append("// Delay the test for the given time in milliseconds before proceeding\n");
				b.append("testContext.delayTest(" + testAction.getDelayBefore() + ");\n\n");
			}

			b.append(GUITestActionGeneratorFactory.getGUITestActionGenerator(testAction, project, testActionDeclarationMap)
					.createTestAction());

			if (testAction.getDelayAfter() != null && testAction.getDelayAfter() >= 0) {
				b.append("\n// Delay the test for the given time in milliseconds before proceeding\n");
				b.append("testContext.delayTest(" + testAction.getDelayAfter() + ");\n");
			}
		}

		b.append("}\n\n");

		addMethod(methodSignature, b.toString());
	}

}
