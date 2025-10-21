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
package net.codecadenza.eclipse.ui.dialog.testing.integration.attribute;

import net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation;
import net.codecadenza.eclipse.model.testing.IntegrationTestCase;
import net.codecadenza.eclipse.model.testing.IntegrationTestModule;
import net.codecadenza.eclipse.model.testing.TestDataAttribute;
import org.eclipse.swt.widgets.Composite;

/**
 * <p>
 * Data that is necessary to initialize an {@link AbstractTestDataAttributePanel}
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class TestDataAttributePanelData {
	private Composite parent;
	private IntegrationTestModule testModule;
	private IntegrationTestCase testCase;
	private IntegrationMethodTestInvocation methodInvocation;
	private TestDataAttribute testDataAttribute;
	private boolean validationMode;
	private boolean enableExpectedListSize;

	/**
	 * Constructor
	 * @param parent
	 * @param testModule
	 * @param testCase
	 * @param methodInvocation
	 * @param testDataAttribute
	 * @param validationMode
	 * @param enableExpectedListSize
	 */
	public TestDataAttributePanelData(Composite parent, IntegrationTestModule testModule, IntegrationTestCase testCase,
			IntegrationMethodTestInvocation methodInvocation, TestDataAttribute testDataAttribute, boolean validationMode,
			boolean enableExpectedListSize) {
		this.parent = parent;
		this.testModule = testModule;
		this.testCase = testCase;
		this.methodInvocation = methodInvocation;
		this.testDataAttribute = testDataAttribute;
		this.validationMode = validationMode;
		this.enableExpectedListSize = enableExpectedListSize;
	}

	/**
	 * @return the parent composite
	 */
	public Composite getParent() {
		return parent;
	}

	/**
	 * @param parent the parent composite to set
	 */
	public void setParent(Composite parent) {
		this.parent = parent;
	}

	/**
	 * @return the test module
	 */
	public IntegrationTestModule getTestModule() {
		return testModule;
	}

	/**
	 * @param testModule the test module to set
	 */
	public void setTestModule(IntegrationTestModule testModule) {
		this.testModule = testModule;
	}

	/**
	 * @return the test case
	 */
	public IntegrationTestCase getTestCase() {
		return testCase;
	}

	/**
	 * @param testCase the test case to set
	 */
	public void setTestCase(IntegrationTestCase testCase) {
		this.testCase = testCase;
	}

	/**
	 * @return the method invocation
	 */
	public IntegrationMethodTestInvocation getMethodInvocation() {
		return methodInvocation;
	}

	/**
	 * @param methodInvocation the method invocation to set
	 */
	public void setMethodInvocation(IntegrationMethodTestInvocation methodInvocation) {
		this.methodInvocation = methodInvocation;
	}

	/**
	 * @return the test data attribute
	 */
	public TestDataAttribute getTestDataAttribute() {
		return testDataAttribute;
	}

	/**
	 * @param testDataAttribute the test data attribute to set
	 */
	public void setTestDataAttribute(TestDataAttribute testDataAttribute) {
		this.testDataAttribute = testDataAttribute;
	}

	/**
	 * @return true if the validation mode should be used
	 */
	public boolean isValidationMode() {
		return validationMode;
	}

	/**
	 * @param validationMode the validation mode flag to set
	 */
	public void setValidationMode(boolean validationMode) {
		this.validationMode = validationMode;
	}

	/**
	 * @return true if the components for the expected list size should be enabled
	 */
	public boolean isEnableExpectedListSize() {
		return enableExpectedListSize;
	}

	/**
	 * @param enableExpectedListSize
	 */
	public void setEnableExpectedListSize(boolean enableExpectedListSize) {
		this.enableExpectedListSize = enableExpectedListSize;
	}

}
