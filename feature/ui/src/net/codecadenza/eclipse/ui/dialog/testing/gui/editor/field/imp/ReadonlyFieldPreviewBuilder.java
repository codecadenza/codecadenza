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

import net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration;
import net.codecadenza.eclipse.model.testing.GUITestData;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Preview generator for fields that are generally read-only (e.g. labels, links...)
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ReadonlyFieldPreviewBuilder extends AbstractGUITestFieldPreviewBuilder {
	private Text txtInput;

	/**
	 * Constructor
	 * @param testData
	 * @param formPanel
	 * @param maintainTestData
	 * @param enableDatabaseLookup
	 */
	public ReadonlyFieldPreviewBuilder(GUITestData testData, Composite formPanel, boolean maintainTestData,
			boolean enableDatabaseLookup) {
		super(testData, formPanel, maintainTestData, enableDatabaseLookup);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.dialog.testing.gui.editor.field.imp.AbstractGUITestFieldPreviewBuilder#getFieldDisplayValue()
	 */
	@Override
	public String getFieldDisplayValue() {
		return testData.getExpectedValue() != null ? testData.getExpectedValue() : "";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.dialog.testing.gui.editor.field.imp.AbstractGUITestFieldPreviewBuilder#
	 * setFieldValue(java.lang.String)
	 */
	@Override
	public void setFieldValue(String value) {
		testData.setExpectedValue(value);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.preview.field.imp.AbstractFieldPreviewBuilder#
	 * getFieldPreview(org.eclipse.swt.widgets.Control)
	 */
	@Override
	public Control getFieldPreview(Control lblField) {
		if (formField.getFieldType() == FormFieldTypeEnumeration.MULTI_LINE_LABEL) {
			if (maintainTestData && addProposalTextField())
				txtInput = initProposalTextField(SWT.BORDER | SWT.MULTI).getControl();
			else
				txtInput = new Text(formPanel, SWT.BORDER | SWT.MULTI);

			lblField.setLayoutData(new GridData(SWT.LEFT, SWT.TOP, false, false));
		}
		else if (maintainTestData && addProposalTextField())
			txtInput = initProposalTextField(SWT.BORDER).getControl();
		else
			txtInput = new Text(formPanel, SWT.BORDER);

		if (!maintainTestData) {
			txtInput.setEnabled(false);

			return txtInput;
		}

		txtInput.setText(getFieldDisplayValue());
		txtInput.setBackground(getControlBackgroundColor());

		txtInput.addKeyListener(new KeyAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.KeyAdapter#keyReleased(org.eclipse.swt.events.KeyEvent)
			 */
			@Override
			public void keyReleased(KeyEvent e) {
				final String textEntered = txtInput.getText();

				setFieldValue(textEntered);
				txtInput.setBackground(getControlBackgroundColor());
			}
		});

		return txtInput;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.dialog.testing.gui.editor.field.imp.AbstractGUITestFieldPreviewBuilder#onClearInput()
	 */
	@Override
	public void onClearInput() {
		txtInput.setText("");
		txtInput.setBackground(getControlBackgroundColor());
	}

}
