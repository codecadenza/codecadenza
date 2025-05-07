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
package net.codecadenza.eclipse.ui.preview.field.imp;

import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration;
import net.codecadenza.eclipse.ui.preview.VisualFormEditorPreviewBuilder;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Preview generator for label fields
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class LabelPreviewBuilder extends AbstractVisualEditorFieldPreviewBuilder {
	/**
	 * Constructor
	 * @param previewBuilder
	 * @param formField
	 * @param formPanel
	 */
	public LabelPreviewBuilder(VisualFormEditorPreviewBuilder previewBuilder, FormField formField, Composite formPanel) {
		super(previewBuilder, formField, formPanel);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.preview.field.imp.AbstractFieldPreviewBuilder#
	 * getFieldPreview(org.eclipse.swt.widgets.Control)
	 */
	@Override
	public Control getFieldPreview(Control lblField) {
		if (formField.getFieldType() == FormFieldTypeEnumeration.MULTI_LINE_LABEL) {
			// For multi-line labels we use a text control as we want to see a border when generating the preview!
			final var txtInput = new Text(formPanel, SWT.BORDER | SWT.READ_ONLY);
			txtInput.setBackground(Display.getCurrent().getSystemColor(SWT.COLOR_WIDGET_BACKGROUND));

			lblField.setLayoutData(new GridData(SWT.LEFT, SWT.TOP, false, false));

			return txtInput;
		}

		final var lblValue = new Label(formPanel, SWT.NONE);
		lblValue.setText("Field value");

		return lblValue;
	}

}
