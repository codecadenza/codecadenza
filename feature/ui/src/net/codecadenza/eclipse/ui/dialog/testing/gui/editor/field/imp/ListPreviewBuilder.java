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

import static net.codecadenza.eclipse.shared.Constants.IMG_ARROW_LEFT;
import static net.codecadenza.eclipse.shared.Constants.IMG_ARROW_RIGHT;
import static net.codecadenza.eclipse.shared.Constants.IMG_SEARCH;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration;
import net.codecadenza.eclipse.model.testing.GUITestData;
import net.codecadenza.eclipse.resource.CodeCadenzaResourcePlugin;
import net.codecadenza.eclipse.shared.dialog.AbstractProposalInputDialog;
import net.codecadenza.eclipse.tools.tdp.GUITestDataProposalService;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Preview generator for list fields
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ListPreviewBuilder extends AbstractGUITestFieldPreviewBuilder {
	private static final String ITEM_DELIMITER = ";";

	private List listSelectedItems;
	private List listExpectedItems;

	/**
	 * Constructor
	 * @param testData
	 * @param formPanel
	 * @param maintainTestData
	 * @param enableDatabaseLookup
	 */
	public ListPreviewBuilder(GUITestData testData, Composite formPanel, boolean maintainTestData, boolean enableDatabaseLookup) {
		super(testData, formPanel, maintainTestData, enableDatabaseLookup);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.dialog.testing.gui.editor.field.imp.AbstractGUITestFieldPreviewBuilder#getFieldDisplayValue()
	 */
	@Override
	public String getFieldDisplayValue() {
		return testData.getFilterValue() != null ? testData.getFilterValue() : "";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.dialog.testing.gui.editor.field.imp.AbstractGUITestFieldPreviewBuilder#setFieldValue(java.
	 * lang.String)
	 */
	@Override
	public void setFieldValue(String value) {
		testData.setFilterValue(value);
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * net.codecadenza.eclipse.ui.preview.field.imp.AbstractFieldPreviewBuilder#getFieldPreview(org.eclipse.swt.widgets.Control)
	 */
	@Override
	public Control getFieldPreview(Control lblField) {
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.dialog.testing.gui.editor.field.imp.AbstractGUITestFieldPreviewBuilder#addMenu(org.eclipse.
	 * swt.widgets.Control)
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

		final var panList = new Composite(formPanel, SWT.NONE);
		panList.setLayout(new GridLayout(3, false));

		if (formField.getFieldType() == FormFieldTypeEnumeration.SEARCHABLE_LIST) {
			final var gdSearch = new GridData(SWT.FILL, SWT.FILL, false, false, 3, 1);

			final var groupSearch = new Group(panList, SWT.NONE);
			groupSearch.setLayoutData(gdSearch);
			groupSearch.setLayout(new GridLayout(3, false));

			final var lblSearchItems = new Label(groupSearch, SWT.NONE);
			lblSearchItems.setText("Search items");

			final Text txtSearchInput = initProposalTextField(groupSearch, SWT.BORDER).getControl();
			txtSearchInput.setLayoutData(new GridData(195, SWT.DEFAULT));
			txtSearchInput.setText(getFieldDisplayValue());
			txtSearchInput.setEnabled(maintainTestData);

			if (!txtSearchInput.getText().isEmpty())
				txtSearchInput.setBackground(colorInputEntered);

			txtSearchInput.addKeyListener(new KeyAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.KeyAdapter#keyReleased(org.eclipse.swt.events.KeyEvent)
				 */
				@Override
				public void keyReleased(KeyEvent e) {
					final String textEntered = txtSearchInput.getText();

					if (!textEntered.isEmpty()) {
						txtSearchInput.setBackground(colorInputEntered);
						setFieldValue(textEntered);
					}
					else {
						txtSearchInput.setBackground(Display.getCurrent().getSystemColor(SWT.COLOR_WHITE));
						setFieldValue(null);
					}
				}
			});

			final var lblPerformSearch = new Label(groupSearch, SWT.NONE);
			lblPerformSearch.setImage(CodeCadenzaResourcePlugin.getImage(IMG_SEARCH));
		}

		final var lblSelectedItems = new Label(panList, SWT.NONE);
		lblSelectedItems.setText("Items to be selected:");

		new Label(panList, SWT.NONE);

		final var lblExpectedItems = new Label(panList, SWT.NONE);
		lblExpectedItems.setText("Items to be checked:");

		listSelectedItems = new List(panList, SWT.V_SCROLL | SWT.MULTI | SWT.H_SCROLL | SWT.BORDER);
		listSelectedItems.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		if (maintainTestData)
			listSelectedItems.addMouseListener(mouseDoubleClickAdapter);
		else
			listSelectedItems.setEnabled(false);

		if (testData.getNewValue() != null) {
			listSelectedItems.setItems(testData.getNewValue().split(ITEM_DELIMITER));
			listSelectedItems.setBackground(colorInputEntered);
		}

		final var panButtons = new Composite(panList, SWT.NONE);
		panButtons.setLayout(new GridLayout());

		final var cmdSelect = new Button(panButtons, SWT.NONE);
		cmdSelect.setImage(CodeCadenzaResourcePlugin.getImage(IMG_ARROW_RIGHT));
		cmdSelect.setSize(32, 32);

		final var cmdDeselect = new Button(panButtons, SWT.NONE);
		cmdDeselect.setImage(CodeCadenzaResourcePlugin.getImage(IMG_ARROW_LEFT));

		listExpectedItems = new List(panList, SWT.V_SCROLL | SWT.MULTI | SWT.H_SCROLL | SWT.BORDER);
		listExpectedItems.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		if (maintainTestData)
			listExpectedItems.addMouseListener(mouseDoubleClickAdapter);
		else
			listExpectedItems.setEnabled(false);

		if (testData.getExpectedValue() != null) {
			listExpectedItems.setItems(testData.getExpectedValue().split(ITEM_DELIMITER));
			listExpectedItems.setBackground(colorValidateField);
		}

		if (columnSpan > 0)
			panList.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, columnSpan, 1));
		else
			panList.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		if (!span && formField.getColIndex() == 1)
			fillEmptyColumns();

		if (maintainTestData)
			addMenus();

		// Add a tool tip
		addToolTipText(listSelectedItems);
	}

	/**
	 * Create menus for both list controls
	 */
	private void addMenus() {
		final var mniSelectedItems = new Menu(listSelectedItems);

		final var mniAddSelectedItem = new MenuItem(mniSelectedItems, SWT.NONE);
		mniAddSelectedItem.setText("Add item");

		mniAddSelectedItem.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final AbstractProposalInputDialog<String> dlg = initInputDialog("Add item to be selected");
				final int returnCode = dlg.open();

				if (returnCode == Window.OK && !dlg.getInputText().isEmpty()) {
					listSelectedItems.add(dlg.getInputText());
					listSelectedItems.setBackground(colorInputEntered);

					testData.setNewValue(convertArrayToItemString(listSelectedItems.getItems()));
				}
			}
		});

		final var mniRemoveSelectedItem = new MenuItem(mniSelectedItems, SWT.NONE);
		mniRemoveSelectedItem.setText("Remove item");

		mniRemoveSelectedItem.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final int selectedItemIndex = listSelectedItems.getSelectionIndex();

				if (selectedItemIndex < 0)
					return;

				final ArrayList<String> list = new ArrayList<>(Arrays.asList(listSelectedItems.getItems()));
				list.remove(selectedItemIndex);

				listSelectedItems.setItems(list.toArray(new String[list.size()]));

				if (list.isEmpty())
					listSelectedItems.setBackground(Display.getCurrent().getSystemColor(SWT.COLOR_WHITE));

				testData.setNewValue(convertArrayToItemString(listSelectedItems.getItems()));
			}
		});

		listSelectedItems.setMenu(mniSelectedItems);

		final var mnuExpectedItems = new Menu(listSelectedItems);

		final var mniAddExpectedItem = new MenuItem(mnuExpectedItems, SWT.NONE);
		mniAddExpectedItem.setText("Add item");

		mniAddExpectedItem.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final AbstractProposalInputDialog<String> dlg = initInputDialog("Add expected item");
				final int returnCode = dlg.open();

				if (returnCode == Window.OK && !dlg.getInputText().isEmpty()) {
					listExpectedItems.add(dlg.getInputText());
					listExpectedItems.setBackground(colorValidateField);

					testData.setExpectedValue(convertArrayToItemString(listExpectedItems.getItems()));
				}
			}
		});

		final var mniRemoveExpectedItem = new MenuItem(mnuExpectedItems, SWT.NONE);
		mniRemoveExpectedItem.setText("Remove item");

		mniRemoveExpectedItem.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final int selectedItemIndex = listExpectedItems.getSelectionIndex();

				if (selectedItemIndex < 0)
					return;

				final ArrayList<String> list = new ArrayList<>(Arrays.asList(listExpectedItems.getItems()));
				list.remove(selectedItemIndex);

				listExpectedItems.setItems(list.toArray(new String[list.size()]));

				if (list.isEmpty())
					listExpectedItems.setBackground(Display.getCurrent().getSystemColor(SWT.COLOR_WHITE));

				testData.setExpectedValue(convertArrayToItemString(listExpectedItems.getItems()));
			}
		});

		listExpectedItems.setMenu(mnuExpectedItems);
	}

	/**
	 * Initialize the proposal input dialog for creating new list items
	 * @param title
	 * @return the dialog
	 */
	protected AbstractProposalInputDialog<String> initInputDialog(String title) {
		return new AbstractProposalInputDialog<>(shell, title, "Enter the item name:", "") {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.eclipse.shared.dialog.AbstractProposalInputDialog#getProposalData(java.lang.String)
			 */
			@Override
			public Collection<String> getProposalData(String filter) {
				if (filter.isEmpty())
					return Collections.emptyList();

				return GUITestDataProposalService.searchProposals(filter, formField, enableDatabaseLookup);
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.eclipse.shared.dialog.AbstractProposalInputDialog#getProposalLabel(java.lang.Object)
			 */
			@Override
			public String getProposalLabel(String element) {
				return element;
			}
		};
	}

	/**
	 * Convert a String array into an internal list representation for a test data object
	 * @param items
	 * @return a string that can be used to fill respective test data fields. It will return null if the array is empty!
	 */
	private String convertArrayToItemString(String[] items) {
		if (items == null || items.length == 0)
			return null;

		return Arrays.asList(items).stream().reduce((a, b) -> a + ITEM_DELIMITER + b).orElse(null);
	}

}
