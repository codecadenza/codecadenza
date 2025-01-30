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

import net.codecadenza.eclipse.model.testing.GUITestData;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;

/**
 * <p>
 * Preview generator for checkbox fields
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CheckboxPreviewBuilder extends AbstractGUITestFieldPreviewBuilder {
	/**
	 * Constructor
	 * @param testData
	 * @param formPanel
	 * @param maintainTestData
	 */
	public CheckboxPreviewBuilder(GUITestData testData, Composite formPanel, boolean maintainTestData) {
		super(testData, formPanel, maintainTestData, false);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.preview.field.imp.AbstractFieldPreviewBuilder#getFieldPreview(org.eclipse.swt.widgets.Control)
	 */
	@Override
	public Control getFieldPreview(Control lblField) {
		if (!maintainTestData) {
			final var chkField = new Button(formPanel, SWT.CHECK);
			chkField.setEnabled(false);

			return chkField;
		}

		// The option SWT.READ_ONLY must not be used as the field's menu won't be displayed otherwise!
		final var combo = new Combo(formPanel, SWT.BORDER);
		combo.setItems("", Boolean.TRUE.toString(), Boolean.FALSE.toString());
		combo.select(combo.indexOf(getFieldDisplayValue()));
		combo.setBackground(getControlBackgroundColor());

		combo.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final String textEntered = combo.getText();

				if (textEntered.isEmpty())
					setFieldValue(textEntered);
				else
					setFieldValue(Boolean.valueOf(textEntered).toString());

				combo.setBackground(getControlBackgroundColor());
			}
		});

		return combo;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.dialog.testing.gui.editor.field.imp.AbstractGUITestFieldPreviewBuilder#addMenu(org.eclipse.
	 * swt.widgets.Control)
	 */
	@Override
	protected void addMenu(Control control) {
		if (!maintainTestData)
			return;

		final var mnuField = new Menu(control);

		if (editFieldData) {
			// Add a menu item in order to check if the field is selected
			final var mniSelected = new MenuItem(mnuField, SWT.NONE);
			mniSelected.setText("Validate selected");

			mniSelected.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(SelectionEvent e) {
					testData.setExpectedValue(Boolean.TRUE.toString());
				}
			});

			// Add a menu item in order to check if the field is not selected
			final var mniNotSelected = new MenuItem(mnuField, SWT.NONE);
			mniNotSelected.setText("Validate not selected");

			mniNotSelected.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(SelectionEvent e) {
					testData.setExpectedValue(Boolean.FALSE.toString());
				}
			});

			// Add a menu item in order to omit testing the field's selection state
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
				}
			});
		}

		control.setMenu(mnuField);
	}

}
