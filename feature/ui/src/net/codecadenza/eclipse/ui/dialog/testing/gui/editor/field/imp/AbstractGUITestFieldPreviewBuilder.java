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

import static net.codecadenza.eclipse.shared.Constants.MIN_FILTER_LENGTH;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.testing.GUITestAction;
import net.codecadenza.eclipse.model.testing.GUITestActionType;
import net.codecadenza.eclipse.model.testing.GUITestData;
import net.codecadenza.eclipse.tools.tdp.GUITestDataProposalService;
import net.codecadenza.eclipse.ui.preview.field.imp.AbstractFieldPreviewBuilder;
import net.codecadenza.runtime.richclient.eclipse.widget.AbstractProposalTextField;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;

/**
 * <p>
 * Abstract preview builder for fields that are displayed in a GUI test form editor
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractGUITestFieldPreviewBuilder extends AbstractFieldPreviewBuilder {
	protected static final String ITEM_DELIMITER = ";";

	protected final Color colorInputCleared = new Color(shell.getDisplay(), 235, 245, 251);
	protected final Color colorInputEntered = new Color(shell.getDisplay(), 254, 245, 231);
	protected final Color colorValidateField = new Color(shell.getDisplay(), 234, 250, 241);
	protected GUITestData testData;
	protected GUITestAction testAction;
	protected boolean editFieldData;
	protected boolean validateFieldData;
	protected boolean maintainTestData;
	protected boolean enableDatabaseLookup;

	/**
	 * Constructor
	 * @param testData
	 * @param formPanel
	 * @param maintainTestData
	 * @param enableDatabaseLookup
	 */
	protected AbstractGUITestFieldPreviewBuilder(GUITestData testData, Composite formPanel, boolean maintainTestData,
			boolean enableDatabaseLookup) {
		super(testData.getFormField(), formPanel);

		this.testData = testData;
		this.testAction = testData.getTestAction();
		this.maintainTestData = maintainTestData;
		this.enableDatabaseLookup = enableDatabaseLookup;
		this.editFieldData = !formField.isReadonly() && testAction.getType() == GUITestActionType.ENTER_FORM_DATA;
		this.validateFieldData = !editFieldData || testAction.getType() == GUITestActionType.VALIDATE_FORM_DATA;
	}

	/**
	 * @return the value that should be displayed in the corresponding form field
	 */
	public String getFieldDisplayValue() {
		if (validateFieldData)
			return testData.getExpectedValue() != null ? testData.getExpectedValue() : "";

		return testData.getNewValue() != null ? testData.getNewValue() : "";
	}

	/**
	 * Set either the expected or the new field value depending on the test action type
	 * @param value
	 */
	public void setFieldValue(String value) {
		if (validateFieldData) {
			if (value.isEmpty())
				testData.setExpectedValue(null);
			else
				testData.setExpectedValue(value);
		}
		else if (value.isEmpty())
			testData.setNewValue(null);
		else
			testData.setNewValue(value);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.preview.field.imp.AbstractFieldPreviewBuilder#addFieldListeners(org.eclipse.swt.widgets.
	 * Control, org.eclipse.swt.widgets.Control)
	 */
	@Override
	protected void addFieldListeners(Control label, Control field) {
		// Dispose internally used colors
		field.addDisposeListener(_ -> {
			colorInputCleared.dispose();
			colorInputEntered.dispose();
			colorValidateField.dispose();
		});
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.preview.field.imp.AbstractFieldPreviewBuilder#addMenu(org.eclipse.swt.widgets.Control)
	 */
	@Override
	protected void addMenu(Control control) {
		if (!maintainTestData)
			return;

		final var mnuField = new Menu(control);

		if (editFieldData) {
			final var mniClearInput = new MenuItem(mnuField, SWT.NONE);
			mniClearInput.setText("Clear input");

			// Add a menu item to clear the field's value
			mniClearInput.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(SelectionEvent e) {
					final FormField formField = testData.getFormField();

					// A space character must be used to reset the selection of a combobox!
					if (formField.getFieldType() == FormFieldTypeEnumeration.COMBOBOX)
						testData.setNewValue(" ");
					else
						testData.setNewValue("");

					onClearInput();
				}
			});

			// Add a menu item to omit changing the field's value when running the test
			final var mniResetInput = new MenuItem(mnuField, SWT.NONE);
			mniResetInput.setText("Reset input");

			mniResetInput.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(SelectionEvent e) {
					testData.setNewValue(null);

					onClearInput();
				}
			});

			// Add a menu item in order to specify an expected value for a given field
			final var mniSetExpectedValue = new MenuItem(mnuField, SWT.NONE);
			mniSetExpectedValue.setText("Set expected value");

			mniSetExpectedValue.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(SelectionEvent e) {
					final var dlg = new InputDialog(shell, "Add expected value", "Enter the expected field value:",
							testData.getExpectedValue(), null);
					final int returnCode = dlg.open();

					if (dlg.getValue() == null || returnCode != Window.OK)
						return;

					// If the dialog provides an empty string a corresponding test should verify that the field is also empty!
					testData.setExpectedValue(dlg.getValue());
				}
			});
		}
		else {
			// Add a menu item to validate if the value of a field is empty
			final var mniValidateEmpty = new MenuItem(mnuField, SWT.NONE);
			mniValidateEmpty.setText("Validate empty");

			mniValidateEmpty.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(SelectionEvent e) {
					testData.setExpectedValue("");

					onClearInput();
				}
			});
		}

		// Add a menu item in order omit testing the field's value
		final var mniResetExpectedValue = new MenuItem(mnuField, SWT.NONE);
		mniResetExpectedValue.setText("Reset expected value");

		mniResetExpectedValue.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				testData.setExpectedValue(null);

				if (validateFieldData)
					onClearInput();
			}
		});

		control.setMenu(mnuField);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.preview.field.imp.AbstractFieldPreviewBuilder#fillEmptyColumns()
	 */
	@Override
	protected void fillEmptyColumns() {
		final int rowIndex = getRowIndexOfEmptyColumn();

		if (rowIndex <= 0)
			return;

		addFillLabel(formPanel);
	}

	/**
	 * Callback method to inform the receiver that the control's input should be cleared
	 */
	public void onClearInput() {

	}

	/**
	 * @return a background color that indicates if a field should be either validated, cleared or set
	 */
	protected Color getControlBackgroundColor() {
		if (editFieldData && testData.getNewValue() != null) {
			if (testData.getNewValue().isEmpty())
				return colorInputCleared;

			return colorInputEntered;
		}

		if (validateFieldData && testData.getExpectedValue() != null)
			return colorValidateField;

		// If no special test operation should be performed a default color should be returned!
		if (formField.isReadonly())
			return Display.getCurrent().getSystemColor(SWT.COLOR_INFO_BACKGROUND);

		return Display.getCurrent().getSystemColor(SWT.COLOR_WHITE);
	}

	/**
	 * @param parentElement
	 * @param style
	 * @return a text field that provides test data proposals
	 */
	protected AbstractProposalTextField<String> initProposalTextField(Composite parentElement, int style) {
		return new AbstractProposalTextField<>(parentElement, style, MIN_FILTER_LENGTH) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.AbstractProposalTextField#getProposalData(java.lang.String)
			 */
			@Override
			public Collection<String> getProposalData(String filter) {
				if (filter.isEmpty())
					return Collections.emptyList();

				return GUITestDataProposalService.searchProposals(filter, formField, enableDatabaseLookup);
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.AbstractProposalTextField#getProposalLabel(java.lang.Object)
			 */
			@Override
			public String getProposalLabel(String element) {
				return element;
			}
		};
	}

	/**
	 * @param style
	 * @return a text field that provides test data proposals
	 */
	protected AbstractProposalTextField<String> initProposalTextField(int style) {
		return initProposalTextField(formPanel, style);
	}

	/**
	 * @return true if a proposal text field should be added
	 */
	protected boolean addProposalTextField() {
		// If the field references a list DTO it might be the case that the domain attribute is null!
		if (formField.getDTOAttribute().getDomainAttribute() == null)
			return true;

		final JavaType type = formField.getDTOAttribute().getDomainAttribute().getJavaType();

		return type.isString() || type.isIntegerOrLong() || type.isUUID();
	}

	/**
	 * Convert a String array into an internal list representation for a test data object
	 * @param items
	 * @return a string that can be used to fill respective test data fields. It will return null if the array is empty!
	 */
	protected String convertArrayToItemString(String[] items) {
		if (items == null || items.length == 0)
			return null;

		return Arrays.asList(items).stream().reduce((a, b) -> a + ITEM_DELIMITER + b).orElse(null);
	}

	/**
	 * Add a label to fill empty columns in a grid layout
	 * @param panParent
	 */
	private void addFillLabel(Composite panParent) {
		final var lblFill = new Label(panParent, SWT.NONE);
		lblFill.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 2, 1));
	}

}
