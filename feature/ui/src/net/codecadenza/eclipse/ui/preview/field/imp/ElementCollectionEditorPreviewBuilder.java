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
import net.codecadenza.eclipse.ui.preview.VisualFormEditorPreviewBuilder;
import net.codecadenza.eclipse.ui.preview.dnd.FormFieldDragSourceListener;
import net.codecadenza.eclipse.ui.preview.dnd.FormFieldDropTargetListener;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
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
public class ElementCollectionEditorPreviewBuilder extends AbstractVisualEditorFieldPreviewBuilder {
	/**
	 * Constructor
	 * @param previewBuilder
	 * @param formField
	 * @param formPanel
	 */
	public ElementCollectionEditorPreviewBuilder(VisualFormEditorPreviewBuilder previewBuilder, FormField formField,
			Composite formPanel) {
		super(previewBuilder, formField, formPanel);
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
		if (fieldCount > 1) {
			final Control lblField = addFieldLabel();

			// Register listeners for moving a form field by using the respective label
			new FormFieldDragSourceListener(lblField, formField);
			new FormFieldDropTargetListener(lblField, formField, previewBuilder);
		}

		// Determine the number of columns that the control will take up
		if (formField.isSpanCols() && formField.getColIndex() == 1)
			columnSpan = fieldCount > 1 ? 3 : 4;
		else if (fieldCount == 1)
			columnSpan = 4;

		formPanel.setLayout(new GridLayout(1, false));

		if (!formField.isReadonly()) {
			final GridLayout glAdd = new GridLayout(3, false);
			glAdd.marginWidth = 0;
			glAdd.marginHeight = 0;
			glAdd.horizontalSpacing = 10;
			glAdd.verticalSpacing = 10;

			final var panAdd = new Composite(formPanel, SWT.NONE);
			panAdd.setLayoutData(new GridData(SWT.FILL, SWT.NONE, true, false));
			panAdd.setLayout(glAdd);

			final var lblAdd = new Label(panAdd, SWT.NONE);
			lblAdd.setText("Value for a new element");

			new FormFieldDragSourceListener(lblAdd, formField);
			new FormFieldDropTargetListener(lblAdd, formField, previewBuilder);

			final var txtNewElementName = new Text(panAdd, SWT.BORDER);
			txtNewElementName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
			txtNewElementName.setEditable(false);
			txtNewElementName.addMouseListener(mouseDoubleClickAdapter);

			addMenu(txtNewElementName);

			final var cmdAdd = new Button(panAdd, SWT.NONE);
			cmdAdd.setText("Add");
		}

		if (formField.isSpanCols() && formField.getColIndex() == 1) {
			final var lblGrid = new Label(formPanel, SWT.NONE);
			lblGrid.setText("Values of the element collection");
			lblGrid.setLayoutData(new GridData(SWT.FILL, SWT.NONE, true, false));

			// Register listeners for moving the field by using the label
			new FormFieldDragSourceListener(lblGrid, formField);
			new FormFieldDropTargetListener(lblGrid, formField, previewBuilder);
		}

		final var listElements = new List(formPanel, SWT.V_SCROLL | SWT.MULTI | SWT.H_SCROLL | SWT.BORDER);
		listElements.addMouseListener(mouseDoubleClickAdapter);
		listElements.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		if (columnSpan > 0)
			formPanel.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, columnSpan, 1));
		else
			formPanel.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		if (!span && formField.getColIndex() == 1)
			fillEmptyColumns();

		addMenu(listElements);

		// Add a tool tip
		addToolTipText(listElements);
	}

}
