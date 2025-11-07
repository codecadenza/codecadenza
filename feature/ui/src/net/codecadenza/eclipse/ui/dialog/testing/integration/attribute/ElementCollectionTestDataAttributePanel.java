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

import java.util.ArrayList;
import java.util.Arrays;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.testing.TestDataAttribute;
import net.codecadenza.eclipse.model.testing.TestDataObject;
import net.codecadenza.eclipse.model.testing.TestingFactory;
import net.codecadenza.eclipse.shared.Constants;
import net.codecadenza.runtime.conversion.ValueConverter;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Panel for test data attributes that are mapped to an element collection
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ElementCollectionTestDataAttributePanel extends AbstractTestDataAttributePanel {
	private Text txtNewElement;
	private List listElements;

	/**
	 * Constructor
	 * @param initializationData
	 */
	public ElementCollectionTestDataAttributePanel(TestDataAttributePanelData initializationData) {
		super(initializationData);

		initPanel();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.dialog.testing.integration.attribute.AbstractTestDataAttributePanel#initPanel()
	 */
	@Override
	public void initPanel() {
		super.initPanel();

		final var panAdd = new Composite(this, SWT.NONE);
		panAdd.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		panAdd.setLayout(new GridLayout(3, false));

		final var lblAdd = new Label(panAdd, SWT.NONE);
		lblAdd.setText("Value for a new element");

		txtNewElement = new Text(panAdd, SWT.BORDER);
		txtNewElement.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtNewElement.setEditable(isEditable());

		final Button cmdAdd = new Button(panAdd, SWT.NONE);
		cmdAdd.setText("Add");
		cmdAdd.setEnabled(isEditable());

		cmdAdd.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (txtNewElement.getText().isEmpty())
					return;

				final JavaType elementType = testDataAttribute.getMappingAttribute().getDomainAttribute().getJavaType();
				final String className;

				if (elementType.getNamespace() != null)
					className = elementType.getNamespace().toString() + "." + elementType.getName();
				else
					className = Constants.PACK_JAVA_LANG + "." + elementType.getName();

				try {
					final Class<?> targetType = Class.forName(className);

					final var valueConverter = new ValueConverter<>(testModule.getDecimalFormat(), testModule.getDecimalSeparator(),
							testModule.getGroupingSeparator(), testModule.getDateTimeFormat(), testModule.getDateFormat(), targetType);

					// Check if the entered value string can be converted to the target type
					valueConverter.convertToValue(txtNewElement.getText());
				}
				catch (final Exception ex) {
					MessageDialog.openInformation(getShell(), "Add new element", "The value could not be converted to " + className + "!");
					return;
				}

				final ArrayList<String> elements = new ArrayList<>(Arrays.asList(listElements.getItems()));
				elements.add(txtNewElement.getText());

				listElements.setItems(elements.toArray(new String[elements.size()]));
			}
		});

		listElements = new List(this, SWT.V_SCROLL | SWT.MULTI | SWT.H_SCROLL | SWT.BORDER);
		listElements.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		listElements.setToolTipText(createToolTipText());

		if (!testDataAttribute.getReferencedObjects().isEmpty()) {
			final String[] elements = new String[testDataAttribute.getReferencedObjects().size()];
			int i = 0;

			for (final TestDataObject testDataObject : testDataAttribute.getReferencedObjects()) {
				// We assume that the test data object contains exactly one test data attribute!
				final TestDataAttribute elementAttribute = testDataObject.getAttributes().getFirst();
				elements[i++] = elementAttribute.getValue();
			}

			listElements.setItems(elements);
		}

		setBackgroundColor(listElements);
		addMenu();
	}

	/**
	 * Add a menu to the list control
	 */
	private void addMenu() {
		final var mnuElements = new Menu(listElements);

		final var mniRemoveSelectedItem = new MenuItem(mnuElements, SWT.NONE);
		mniRemoveSelectedItem.setText("Remove");

		mniRemoveSelectedItem.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final int selectedItemIndex = listElements.getSelectionIndex();

				if (selectedItemIndex < 0)
					return;

				final ArrayList<String> elements = new ArrayList<>(Arrays.asList(listElements.getItems()));
				elements.remove(selectedItemIndex);

				listElements.setItems(elements.toArray(new String[elements.size()]));
			}
		});

		listElements.setMenu(mnuElements);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.dialog.testing.integration.attribute.AbstractTestDataAttributePanel#validateAndApplyInput()
	 */
	@Override
	public void validateAndApplyInput() {
		testDataAttribute.setOperator(getSelectedOperator());
		testDataAttribute.getReferencedObjects().clear();

		for (final String item : listElements.getItems()) {
			final TestDataAttribute elementValueAttribute = TestingFactory.eINSTANCE.createTestDataAttribute();
			elementValueAttribute.setValue(item);

			final TestDataObject elementValueObject = TestingFactory.eINSTANCE.createTestDataObject();
			elementValueObject.getAttributes().add(elementValueAttribute);

			testDataAttribute.getReferencedObjects().add(elementValueObject);
		}
	}

}
