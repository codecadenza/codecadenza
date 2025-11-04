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

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.concurrent.ThreadLocalRandom;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Panel for test data attributes that are mapped to date values
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DateTestDataAttributePanel extends AbstractTestDataAttributePanel {
	private final DateTimeFormatter formatter;
	private String formatPattern;
	private Text txtValue;

	/**
	 * Constructor
	 * @param initializationData
	 */
	public DateTestDataAttributePanel(TestDataAttributePanelData initializationData) {
		super(initializationData);

		if (testDataAttribute.getJavaType().isLocalDate())
			this.formatPattern = testModule.getDateFormat();
		else
			this.formatPattern = testModule.getDateTimeFormat();

		this.formatter = DateTimeFormatter.ofPattern(formatPattern).withZone(ZoneId.systemDefault());

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
		txtValue.setToolTipText("Enter a date using format pattern '" + formatPattern + "'");

		if (testDataAttribute.getValue() != null)
			txtValue.setText(testDataAttribute.getValue());

		setBackgroundColor(txtValue);
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
			formatter.parse(txtValue.getText());
		}
		catch (final DateTimeParseException e) {
			txtValue.setFocus();
			throw e;
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

		// Create a random date from last year, starting from today
		final long end = LocalDateTime.now().toEpochSecond(ZoneOffset.UTC);
		final long start = LocalDateTime.now().minusDays(356).toEpochSecond(ZoneOffset.UTC);
		final long randomValue = ThreadLocalRandom.current().nextLong(start, end);

		txtValue.setText(formatter.format(LocalDateTime.ofEpochSecond(randomValue, 0, ZoneOffset.UTC)));
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

}
