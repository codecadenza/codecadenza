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

import java.util.concurrent.ThreadLocalRandom;
import net.codecadenza.eclipse.model.java.JavaEnum;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Combo;

/**
 * <p>
 * Panel for test data attributes that are mapped to an enumeration
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EnumTestDataAttributePanel extends AbstractTestDataAttributePanel {
	private Combo cboEnum;

	/**
	 * Constructor
	 * @param initializationData
	 */
	public EnumTestDataAttributePanel(TestDataAttributePanelData initializationData) {
		super(initializationData);

		initPanel();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.dialog.testing.integration.panel.attribute.AbstractTestDataAttributePanel#initPanel()
	 */
	@Override
	protected void initPanel() {
		super.initPanel();

		cboEnum = new Combo(this, SWT.READ_ONLY);
		cboEnum.add("");
		cboEnum.setEnabled(isEditable());

		final var enumType = (JavaEnum) testDataAttribute.getJavaType();
		enumType.getEnumerationValues().stream().forEach(e -> cboEnum.add(e.getName()));

		if (testDataAttribute.getValue() != null && !testDataAttribute.getValue().isEmpty())
			cboEnum.select(cboEnum.indexOf(testDataAttribute.getValue()));

		setBackgroundColor(cboEnum);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.dialog.testing.integration.panel.attribute.AbstractTestDataAttributePanel#
	 * validateAndApplyInput()
	 */
	@Override
	public void validateAndApplyInput() {
		final int selectionIndex = cboEnum.getSelectionIndex();

		testDataAttribute.setOperator(getSelectedOperator());

		if (selectionIndex <= 0)
			testDataAttribute.setValue(null);
		else
			testDataAttribute.setValue(cboEnum.getItem(selectionIndex));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.dialog.testing.integration.attribute.AbstractTestDataAttributePanel#onRequestRandomValue()
	 */
	@Override
	public void onRequestRandomValue() {
		if (cboEnum.getItemCount() <= 1 || cboEnum.getSelectionIndex() != -1)
			return;

		// Take an item starting from index 1 so that the default empty item won't we selected
		final int randomValue = ThreadLocalRandom.current().nextInt(1, cboEnum.getItemCount());

		cboEnum.select(randomValue);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.dialog.testing.integration.attribute.AbstractTestDataAttributePanel#
	 * onSelectedOperatorChanged(boolean)
	 */
	@Override
	protected void onSelectedOperatorChanged() {
		cboEnum.setEnabled(isEditable());
		setBackgroundColor(cboEnum);

		if (!isEditable())
			cboEnum.select(0);
	}
}
