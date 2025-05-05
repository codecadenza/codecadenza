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
package net.codecadenza.eclipse.ui.dialog.testing.gui.editor.field.imp;

import java.util.ArrayList;
import java.util.Arrays;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.testing.GUITestData;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Preview generator for element collection editor fields
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ElementCollectionEditorPreviewBuilder extends AbstractGUITestFieldPreviewBuilder {
	private List listElements;

	/**
	 * Constructor
	 * @param testData
	 * @param formPanel
	 * @param maintainTestData
	 */
	public ElementCollectionEditorPreviewBuilder(GUITestData testData, Composite formPanel, boolean maintainTestData) {
		super(testData, formPanel, maintainTestData, false);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.preview.field.imp.AbstractFieldPreviewBuilder#
	 * getFieldPreview(org.eclipse.swt.widgets.Control)
	 */
	@Override
	public Control getFieldPreview(Control lblField) {
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.dialog.testing.gui.editor.field.imp.AbstractGUITestFieldPreviewBuilder#
	 * addMenu(org.eclipse.swt.widgets.Control)
	 */
	@Override
	protected void addMenu(Control control) {
		// The menu provided by the base class shouldn't be used!
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.preview.field.imp.AbstractFieldPreviewBuilder#generateFieldPreview()
	 */
	@Override
	public void generateFieldPreview() {
		int fieldCount = 0;
		int columnSpan = 0;

		// Calculate the number of visible form fields
		for (final FormField field : formField.getPanel().getFields())
			if (field.isVisible())
				fieldCount++;

		final boolean span = (formField.isSpanCols() && formField.getColIndex() == 1) || fieldCount == 1;

		if (!span && formField.getColIndex() == 2)
			fillEmptyColumns();

		// Add a field label if the panel contains further visible fields
		if (fieldCount > 1)
			addFieldLabel();

		// Determine the number of columns that the control will take up
		if (formField.isSpanCols() && formField.getColIndex() == 1)
			columnSpan = fieldCount > 1 ? 3 : 4;
		else if (fieldCount == 1)
			columnSpan = 4;

		formPanel.setLayout(new GridLayout(1, false));

		final GridLayout glAdd = new GridLayout(3, false);
		glAdd.marginWidth = 0;
		glAdd.marginHeight = 0;
		glAdd.horizontalSpacing = 10;
		glAdd.verticalSpacing = 10;

		final var panAdd = new Composite(formPanel, SWT.NONE);
		panAdd.setLayoutData(new GridData(SWT.FILL, SWT.NONE, true, false));
		panAdd.setLayout(glAdd);

		final var lblAdd = new Label(panAdd, SWT.NONE);

		if (formType == FormTypeEnumeration.READONLY)
			lblAdd.setText("Value for an expected element");
		else
			lblAdd.setText("Value for a new element");

		final var txtNewElementName = new Text(panAdd, SWT.BORDER);
		txtNewElementName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtNewElementName.setEditable(maintainTestData);

		final var cmdAdd = new Button(panAdd, SWT.NONE);
		cmdAdd.setText("Add");

		cmdAdd.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (txtNewElementName.getText().isEmpty())
					return;

				final ArrayList<String> elements = new ArrayList<>(Arrays.asList(listElements.getItems()));
				elements.add(txtNewElementName.getText());

				updateElements(elements);
			}
		});

		listElements = new List(formPanel, SWT.V_SCROLL | SWT.MULTI | SWT.H_SCROLL | SWT.BORDER);
		listElements.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		if (columnSpan > 0)
			formPanel.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, columnSpan, 1));
		else
			formPanel.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		if (!span && formField.getColIndex() == 1)
			fillEmptyColumns();

		if (formType != FormTypeEnumeration.READONLY && testData.getNewValue() != null) {
			listElements.setItems(testData.getNewValue().split(ITEM_DELIMITER));
			listElements.setBackground(colorInputEntered);
		}
		else if (formType == FormTypeEnumeration.READONLY && testData.getExpectedValue() != null) {
			listElements.setItems(testData.getExpectedValue().split(ITEM_DELIMITER));
			listElements.setBackground(colorValidateField);
		}

		if (maintainTestData)
			addMenu();

		addToolTipText(listElements);
	}

	/**
	 * Create a menu for the list control
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

				updateElements(elements);
			}
		});

		listElements.setMenu(mnuElements);
	}

	/**
	 * Update the list that contains the elements and save the test data
	 * @param elements
	 */
	private void updateElements(java.util.List<String> elements) {
		listElements.setItems(elements.toArray(new String[elements.size()]));

		if (elements.isEmpty())
			listElements.setBackground(Display.getCurrent().getSystemColor(SWT.COLOR_WHITE));

		if (formType == FormTypeEnumeration.READONLY) {
			testData.setExpectedValue(convertArrayToItemString(listElements.getItems()));
			listElements.setBackground(colorValidateField);
		}
		else {
			testData.setNewValue(convertArrayToItemString(listElements.getItems()));
			listElements.setBackground(colorInputEntered);
		}
	}

}
