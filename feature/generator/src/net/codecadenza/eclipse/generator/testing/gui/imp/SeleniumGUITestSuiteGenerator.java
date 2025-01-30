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

import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.testing.TestSuite;

/**
 * <p>
 * Generator for Selenium test suites
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SeleniumGUITestSuiteGenerator extends AbstractJavaSourceGenerator {
	private final TestSuite testSuite;

	/**
	 * Constructor
	 * @param testSuite
	 */
	public SeleniumGUITestSuiteGenerator(TestSuite testSuite) {
		super(testSuite.getSourceFile());

		this.testSuite = testSuite;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importPackage("org.junit.platform.suite.api");
		importPackage("net.codecadenza.runtime.selenium.junit");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("@Suite");

		if (!testSuite.getTestCases().isEmpty()) {
			b.append("@SelectClasses({");
			b.append(testSuite.getTestCases().stream().map(e -> e.getName() + ".class").reduce((r, t) -> r + ", " + t).orElse(""));
			b.append("})\n");
		}

		b.append("public class " + testSuite.getName() + " extends AbstractSeleniumTestSuite");
	}

}
