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

import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.testing.TestDataAttribute;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Panel for test data attributes that are mapped to integer values
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class IntegerTestDataAttributePanel extends AbstractTestDataAttributePanel {
	private Text txtValue;

	/**
	 * Constructor
	 * @param initializationData
	 */
	public IntegerTestDataAttributePanel(TestDataAttributePanelData initializationData) {
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

		txtValue = new Text(this, SWT.BORDER);
		txtValue.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtValue.setEditable(isEditable());

		if (testDataAttribute.getReferencedAttribute() != null)
			txtValue.setText(TRACKING_PREFIX + testDataAttribute.getReferencedAttribute().getId());
		else if (testDataAttribute.getValue() != null)
			txtValue.setText(testDataAttribute.getValue());

		initDropListener(txtValue);
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

		// If necessary, reset the referenced attribute
		if (testDataAttribute.getReferencedAttribute() != null
				&& !txtValue.getText().equals(TRACKING_PREFIX + testDataAttribute.getReferencedAttribute().getId()))
			testDataAttribute.setReferencedAttribute(null);

		if (txtValue.getText().isEmpty() || testDataAttribute.getReferencedAttribute() != null) {
			testDataAttribute.setValue(null);
			return;
		}

		try {
			Integer.parseInt(txtValue.getText());
		}
		catch (final NumberFormatException e) {
			txtValue.setFocus();
			throw e;
		}

		testDataAttribute.setValue(txtValue.getText());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.dialog.testing.integration.attribute.AbstractTestDataAttributePanel#
	 * onSetReference(net.codecadenza.eclipse.model.domain.DomainObject, net.codecadenza.eclipse.model.testing.TestDataAttribute)
	 */
	@Override
	protected void onSetReference(DomainObject domainObject, TestDataAttribute trackedAttribute) {
		if (isEditable() && testDataAttribute.isReferenceAllowed(domainObject)) {
			testDataAttribute.setReferencedAttribute(trackedAttribute);
			testDataAttribute.setValue(null);

			txtValue.setText(TRACKING_PREFIX + trackedAttribute.getId());

			setBackgroundColor(txtValue);
		}
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
