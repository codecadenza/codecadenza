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
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingObject;
import net.codecadenza.eclipse.model.mapping.MappingObject;
import net.codecadenza.eclipse.model.testing.TestDataAttribute;
import net.codecadenza.eclipse.model.testing.TestDataObject;
import net.codecadenza.eclipse.service.testing.integration.IntegrationTestCaseService;
import net.codecadenza.eclipse.ui.dialog.testing.integration.panel.TestDataObjectGridPanel;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Panel for test data attributes that are mapped to objects of a many-to-many association
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ManyToManyTestDataAttributePanel extends AbstractTestDataAttributePanel {
	private Text txtValue;
	private TestDataObjectGridPanel gridTestObjects;

	/**
	 * Constructor
	 * @param initializationData
	 */
	public ManyToManyTestDataAttributePanel(TestDataAttributePanelData initializationData) {
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

		final var panAdd = new Composite(this, SWT.NONE);
		panAdd.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		panAdd.setLayout(new GridLayout(3, false));

		final var lblAdd = new Label(panAdd, SWT.NONE);
		lblAdd.setText("Add object");

		txtValue = new Text(panAdd, SWT.BORDER);
		txtValue.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtValue.setToolTipText(createToolTipText());

		initDropListener(txtValue);

		final Button cmdAdd = new Button(panAdd, SWT.PUSH);
		cmdAdd.setText("Add");

		cmdAdd.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final String itemValue = txtValue.getText();

				// An empty item value is not supported!
				if (itemValue.isEmpty())
					return;

				addItem(itemValue, null);
			}
		});

		final MappingObject mappingObject;

		if (testDataAttribute.getMappingAttribute() instanceof final DTOBeanAttribute dtoAttribute)
			mappingObject = dtoAttribute.getReferencedDTOBean();
		else if (testDataAttribute.getMappingAttribute() instanceof final ExchangeMappingAttribute exchangeMappingAttribute)
			mappingObject = (ExchangeMappingObject) exchangeMappingAttribute.getJavaType();
		else
			throw new IllegalStateException("The mapping object could not be determined!");

		final var gdTestObjects = new GridData(SWT.FILL, SWT.FILL, true, true);
		gdTestObjects.minimumHeight = 300;

		gridTestObjects = new TestDataObjectGridPanel(this, testModule, testCase, methodInvocation, mappingObject, testDataAttribute,
				validationMode, enableExpectedListSize);
		gridTestObjects.setLayoutData(gdTestObjects);
		gridTestObjects.setData(testDataAttribute.getReferencedObjects());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.dialog.testing.integration.panel.attribute.AbstractTestDataAttributePanel#
	 * validateAndApplyInput()
	 */
	@Override
	public void validateAndApplyInput() {
		gridTestObjects.validateAndApplyInput();
		testDataAttribute.setOperator(getSelectedOperator());
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

		if (inheritanceTree.contains(assoc.getTarget()))
			addItem(null, trackedAttribute);
	}

	/**
	 * Add a new item to the list
	 * @param itemValue
	 * @param trackedAttribute
	 */
	private void addItem(String itemValue, TestDataAttribute trackedAttribute) {
		final Shell shell = Display.getCurrent().getActiveShell();
		final TestDataObject testDataObject = new IntegrationTestCaseService(testModule).initReferencedTestObject(testDataAttribute);
		final TestDataAttribute pkAttr = testDataObject.getPKAttribute();

		if (trackedAttribute == null) {
			// Try to set the primary key attribute
			if (pkAttr.getJavaType().isString())
				pkAttr.setValue(itemValue);
			else {
				try {
					if (pkAttr.getJavaType().isIntegerOrLong())
						pkAttr.setValue(Integer.toString(Integer.parseInt(itemValue)));
					else
						pkAttr.setValue(UUID.fromString(itemValue).toString());
				}
				catch (final Exception ex) {
					txtValue.setFocus();
					MessageDialog.openInformation(shell, "Add test data object", ex.getMessage());
					return;
				}
			}
		}
		else
			pkAttr.setReferencedAttribute(trackedAttribute);

		testDataAttribute.getReferencedObjects().add(testDataObject);

		gridTestObjects.setData(testDataAttribute.getReferencedObjects());
	}
}
