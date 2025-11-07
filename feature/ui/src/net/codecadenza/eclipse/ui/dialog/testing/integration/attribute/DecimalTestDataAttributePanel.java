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

import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.ParseException;
import java.util.concurrent.ThreadLocalRandom;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Panel for test data attributes that are mapped to decimal values
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DecimalTestDataAttributePanel extends AbstractTestDataAttributePanel {
	private final DecimalFormat decimalFormat;
	private Text txtValue;

	/**
	 * Constructor
	 * @param initializationData
	 */
	public DecimalTestDataAttributePanel(TestDataAttributePanelData initializationData) {
		super(initializationData);

		final var decimalSymbols = new DecimalFormatSymbols();
		decimalSymbols.setDecimalSeparator(testModule.getDecimalSeparator());
		decimalSymbols.setGroupingSeparator(testModule.getGroupingSeparator());

		this.decimalFormat = new DecimalFormat(testModule.getDecimalFormat());
		this.decimalFormat.setDecimalFormatSymbols(decimalSymbols);

		initPanel();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.dialog.testing.integration.panel.attribute.AbstractTestDataAttributePanel#initPanel()
	 */
	@Override
	protected void initPanel() {
		super.initPanel();

		txtValue = new Text(this, SWT.BORDER);
		txtValue.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtValue.setEditable(isEditable());

		if (testDataAttribute.getValue() != null)
			txtValue.setText(testDataAttribute.getValue());

		setBackgroundColor(txtValue);
		addToolTipText();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.dialog.testing.integration.panel.attribute.AbstractTestDataAttributePanel#
	 * validateAndApplyInput()
	 */
	@Override
	public void validateAndApplyInput() {
		testDataAttribute.setOperator(getSelectedOperator());

		if (txtValue.getText().isEmpty()) {
			testDataAttribute.setValue(null);
			return;
		}

		try {
			decimalFormat.parse(txtValue.getText());
		}
		catch (final ParseException e) {
			txtValue.setFocus();
			throw new NumberFormatException(e.getMessage());
		}

		testDataAttribute.setValue(txtValue.getText());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.dialog.testing.integration.attribute.AbstractTestDataAttributePanel#onRequestRandomValue()
	 */
	@Override
	public void onRequestRandomValue() {
		if (!txtValue.getText().isEmpty())
			return;

		final double randomValue = ThreadLocalRandom.current().nextDouble(0.0, 1000.0);

		txtValue.setText(decimalFormat.format(randomValue));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.dialog.testing.integration.attribute.AbstractTestDataAttributePanel#
	 * onSelectedOperatorChanged()
	 */
	@Override
	protected void onSelectedOperatorChanged() {
		txtValue.setEditable(isEditable());
		setBackgroundColor(txtValue);

		if (!isEditable())
			txtValue.setText("");
	}

	/**
	 * Add a tool tip text to the input field
	 */
	private void addToolTipText() {
		final var toolTipText = new StringBuilder(createToolTipText());
		toolTipText.append("\n");
		toolTipText.append("Enter a decimal number using format '" + testModule.getDecimalFormat() + "'");

		txtValue.setToolTipText(toolTipText.toString());
	}

}
