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

import java.util.Collection;
import java.util.UUID;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.testing.TestDataAttribute;
import net.codecadenza.eclipse.model.testing.TestDataObject;
import net.codecadenza.eclipse.service.testing.integration.IntegrationTestCaseService;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Panel for test data attributes that are mapped to an object of a many-to-one association
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ManyToOneTestDataAttributePanel extends AbstractTestDataAttributePanel {
	private final TestDataObject testDataObject;
	private final TestDataAttribute pkAttr;
	private Text txtValue;

	/**
	 * Constructor
	 * @param initializationData
	 */
	public ManyToOneTestDataAttributePanel(TestDataAttributePanelData initializationData) {
		super(initializationData);

		if (this.testDataAttribute.getReferencedObjects().isEmpty())
			this.testDataObject = new IntegrationTestCaseService(testModule).initReferencedTestObject(this.testDataAttribute);
		else
			this.testDataObject = this.testDataAttribute.getReferencedObjects().getFirst();

		this.pkAttr = this.testDataObject.getPKAttribute();

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
		txtValue.setToolTipText(createToolTipText());

		if (pkAttr.getReferencedAttribute() != null)
			txtValue.setText(TRACKING_PREFIX + pkAttr.getReferencedAttribute().getId());
		else if (pkAttr.getValue() != null) {
			final JavaType pkType = pkAttr.getJavaType();

			// The MIN_VALUE value for setting an optional association to null should not be displayed!
			final boolean hideValue = !testDataAttribute.isMandatory() && pkType.isPrimitive()
					&& ((pkType.isInteger() && pkAttr.getValue().equals(Integer.toString(Integer.MIN_VALUE)))
							|| (pkType.isLong() && pkAttr.getValue().equals(Long.toString(Long.MIN_VALUE))));

			if (!hideValue)
				txtValue.setText(pkAttr.getValue());
		}

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
		pkAttr.setValue(null);

		testDataAttribute.setOperator(getSelectedOperator());

		// If necessary, reset the referenced attribute
		if (pkAttr.getReferencedAttribute() != null
				&& !txtValue.getText().equals(TRACKING_PREFIX + pkAttr.getReferencedAttribute().getId()))
			pkAttr.setReferencedAttribute(null);

		if (txtValue.getText().isEmpty() || pkAttr.getReferencedAttribute() != null) {
			final JavaType pkType = pkAttr.getJavaType();

			if (!testDataAttribute.isMandatory() && pkAttr.getReferencedAttribute() == null && pkType.isPrimitive()
					&& pkType.isIntegerOrLong()) {
				// The MIN_VALUE is used for setting an optional association to null!
				if (pkType.isInteger())
					pkAttr.setValue(Integer.toString(Integer.MIN_VALUE));
				else
					pkAttr.setValue(Long.toString(Long.MIN_VALUE));
			}

			return;
		}

		if (pkAttr.getJavaType().isString())
			pkAttr.setValue(txtValue.getText());
		else if (pkAttr.getJavaType().isIntegerOrLong())
			pkAttr.setValue(Integer.toString(Integer.parseInt(txtValue.getText())));
		else if (pkAttr.getJavaType().isUUID())
			pkAttr.setValue(UUID.fromString(txtValue.getText()).toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.dialog.testing.integration.attribute.AbstractTestDataAttributePanel#
	 * onSetReference(net.codecadenza.eclipse.model.domain.DomainObject, net.codecadenza.eclipse.model.testing.TestDataAttribute)
	 */
	@Override
	protected void onSetReference(DomainObject domainObject, TestDataAttribute trackedAttribute) {
		final AbstractDomainAssociation assoc = testDataAttribute.getMappingAttribute().getAssociation();
		final Collection<DomainObject> inheritanceTree = domainObject.getFullInheritanceTree();

		if (isEditable() && inheritanceTree.contains(assoc.getTarget())) {
			pkAttr.setReferencedAttribute(trackedAttribute);
			pkAttr.setValue(null);

			txtValue.setText(TRACKING_PREFIX + trackedAttribute.getId());

			setBackgroundColor(txtValue);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.dialog.testing.integration.attribute.AbstractTestDataAttributePanel#
	 * setBackgroundColor(org.eclipse.swt.widgets.Control)
	 */
	@Override
	protected void setBackgroundColor(Control control) {
		if (pkAttr.getReferencedAttribute() != null)
			control.setBackground(HAS_REFERENCE_COLOR);
		else if (!isEditable())
			control.setBackground(READONLY_COLOR);
		else
			control.setBackground(STANDARD_COLOR);
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
