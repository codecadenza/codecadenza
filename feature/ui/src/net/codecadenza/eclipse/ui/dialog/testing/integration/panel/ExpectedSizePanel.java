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
package net.codecadenza.eclipse.ui.dialog.testing.integration.panel;

import java.util.UUID;
import net.codecadenza.eclipse.model.testing.AssertionOperator;
import net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation;
import net.codecadenza.eclipse.model.testing.TestDataAttribute;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Panel for entering the expected size
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ExpectedSizePanel extends Composite {
	private final IntegrationMethodTestInvocation methodInvocation;
	private final TestDataAttribute testDataAttribute;
	private final boolean enableOperator;
	private Combo cboOperator;
	private Text txtExpectedSize;

	/**
	 * Create the panel for the expected size of a {@link TestDataAttribute} that is mapped to a list
	 * @param parent the parent composite
	 * @param testDataAttribute the test data attribute
	 * @param label the label to be displayed
	 */
	public ExpectedSizePanel(Composite parent, TestDataAttribute testDataAttribute, String label) {
		super(parent, SWT.NONE);

		this.methodInvocation = null;
		this.testDataAttribute = testDataAttribute;
		this.enableOperator = true;

		initPanel(label);
	}

	/**
	 * Create the panel for the expected size of a list or a file returned by a {@link IntegrationMethodTestInvocation}
	 * @param parent the parent composite
	 * @param methodInvocation the method invocation
	 * @param label the label to be displayed
	 * @param enableOperator flag that controls if an assertion operator can be selected
	 */
	public ExpectedSizePanel(Composite parent, IntegrationMethodTestInvocation methodInvocation, String label,
			boolean enableOperator) {
		super(parent, SWT.NONE);

		this.methodInvocation = methodInvocation;
		this.testDataAttribute = null;
		this.enableOperator = enableOperator;

		initPanel(label);
	}

	/**
	 * Validate the input and apply it
	 */
	public void validateAndApplyInput() {
		final Integer expectedSize = txtExpectedSize.getText().isEmpty() ? null : Integer.parseInt(txtExpectedSize.getText());

		if (expectedSize != null) {
			if (expectedSize < 0)
				throw new IllegalStateException("The expected size must not be negative!");

			if (testDataAttribute != null) {
				testDataAttribute.setExpectedSize(expectedSize);

				if (testDataAttribute.getId() == null)
					testDataAttribute.setId(UUID.randomUUID().toString());
			}
			else
				methodInvocation.setExpectedSize(expectedSize);
		}
		else if (testDataAttribute != null) {
			testDataAttribute.setExpectedSize(null);
			testDataAttribute.setId(null);
		}
		else
			methodInvocation.setExpectedSize(null);

		if (enableOperator) {
			final AssertionOperator operator = AssertionOperator.valueOf(cboOperator.getItem(cboOperator.getSelectionIndex()));

			if (testDataAttribute != null)
				testDataAttribute.setExpectedSizeOperator(operator);
			else
				methodInvocation.setExpectedSizeOperator(operator);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.swt.widgets.Control#setEnabled(boolean)
	 */
	@Override
	public void setEnabled(boolean enabled) {
		super.setEnabled(enabled);

		txtExpectedSize.setEnabled(enabled);

		if (enableOperator)
			cboOperator.setEnabled(enabled);
	}

	/**
	 * Initialize the panel
	 * @param label
	 */
	protected void initPanel(String label) {
		final var glPanel = new GridLayout(3, false);
		glPanel.marginWidth = 0;
		glPanel.marginHeight = 0;

		setLayout(glPanel);

		final var lblLabel = new Label(this, SWT.NONE);
		lblLabel.setText(label);

		final var gdOperator = new GridData(SWT.LEFT, SWT.CENTER, false, false);
		gdOperator.widthHint = 200;

		cboOperator = new Combo(this, SWT.READ_ONLY);
		cboOperator.setLayoutData(gdOperator);
		cboOperator.setEnabled(enableOperator);
		cboOperator.add(AssertionOperator.NONE.name());
		cboOperator.add(AssertionOperator.EQUAL.name());
		cboOperator.add(AssertionOperator.GREATER.name());
		cboOperator.add(AssertionOperator.GREATER_OR_EQUAL.name());
		cboOperator.add(AssertionOperator.SMALLER.name());
		cboOperator.add(AssertionOperator.SMALLER_OR_EQUAL.name());
		cboOperator.select(0);

		// When creating a new nested invocation the assertion operator of the parent invocation won't be displayed!
		if (testDataAttribute != null)
			cboOperator.select(cboOperator.indexOf(testDataAttribute.getExpectedSizeOperator().name()));
		else if (methodInvocation.getParentInvocation() == null)
			cboOperator.select(cboOperator.indexOf(methodInvocation.getExpectedSizeOperator().name()));
		else
			cboOperator.select(cboOperator.indexOf(methodInvocation.getParentInvocation().getExpectedSizeOperator().name()));

		txtExpectedSize = new Text(this, SWT.BORDER);
		txtExpectedSize.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));

		if (testDataAttribute != null && testDataAttribute.getExpectedSize() != null)
			txtExpectedSize.setText(Integer.toString(testDataAttribute.getExpectedSize()));
		else if (methodInvocation != null && methodInvocation.getExpectedSize() != null)
			txtExpectedSize.setText(Integer.toString(methodInvocation.getExpectedSize()));
	}

}
