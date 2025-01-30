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

import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.testing.GUITestData;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

/**
 * <p>
 * Preview generator for components that are mapped to enumeration fields
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EnumComboboxPreviewBuilder extends AbstractGUITestFieldPreviewBuilder {
	private Combo enumCombo;

	/**
	 * Constructor
	 * @param testData
	 * @param formPanel
	 * @param maintainTestData
	 */
	public EnumComboboxPreviewBuilder(GUITestData testData, Composite formPanel, boolean maintainTestData) {
		super(testData, formPanel, maintainTestData, false);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.preview.field.imp.AbstractFieldPreviewBuilder#getFieldPreview(org.eclipse.swt.widgets.Control)
	 */
	@Override
	public Control getFieldPreview(Control lblField) {
		if (!maintainTestData) {
			enumCombo = new Combo(formPanel, SWT.BORDER | SWT.READ_ONLY);
			enumCombo.setEnabled(false);

			return enumCombo;
		}

		enumCombo = new Combo(formPanel, SWT.BORDER);
		enumCombo.setBackground(getControlBackgroundColor());
		enumCombo.add("");

		enumCombo.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final String textEntered = enumCombo.getText();

				setFieldValue(textEntered);
				enumCombo.setBackground(getControlBackgroundColor());
			}
		});

		// Create the combobox items for all literals
		final var fieldEnum = (JavaEnum) formField.getDTOAttribute().getDomainAttribute().getJavaType();
		fieldEnum.getEnumerationValues().stream().forEach(e -> enumCombo.add(e.getName()));

		enumCombo.select(enumCombo.indexOf(getFieldDisplayValue()));

		return enumCombo;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.dialog.testing.gui.editor.field.imp.AbstractGUITestFieldPreviewBuilder#onClearInput()
	 */
	@Override
	public void onClearInput() {
		enumCombo.select(0);
		enumCombo.setBackground(getControlBackgroundColor());
	}

}
