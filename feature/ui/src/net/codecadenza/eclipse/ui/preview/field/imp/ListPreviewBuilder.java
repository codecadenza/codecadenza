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

import static net.codecadenza.eclipse.shared.Constants.IMG_ARROW_LEFT;
import static net.codecadenza.eclipse.shared.Constants.IMG_ARROW_RIGHT;
import static net.codecadenza.eclipse.shared.Constants.IMG_SEARCH;

import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration;
import net.codecadenza.eclipse.resource.CodeCadenzaResourcePlugin;
import net.codecadenza.eclipse.ui.preview.VisualFormEditorPreviewBuilder;
import net.codecadenza.eclipse.ui.preview.dnd.FormFieldDragSourceListener;
import net.codecadenza.eclipse.ui.preview.dnd.FormFieldDropTargetListener;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
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
public class ListPreviewBuilder extends AbstractVisualEditorFieldPreviewBuilder {
	/**
	 * Constructor
	 * @param previewBuilder
	 * @param formField
	 * @param formPanel
	 */
	public ListPreviewBuilder(VisualFormEditorPreviewBuilder previewBuilder, FormField formField, Composite formPanel) {
		super(previewBuilder, formField, formPanel);
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

		final var panList = new Composite(formPanel, SWT.NONE);
		panList.setLayout(new GridLayout(3, false));

		if (formField.getFieldType() == FormFieldTypeEnumeration.SEARCHABLE_LIST) {
			final var gdSearch = new GridData(SWT.FILL, SWT.FILL, false, false, 3, 1);

			final var groupSearch = new Group(panList, SWT.NONE);
			groupSearch.setLayoutData(gdSearch);
			groupSearch.setLayout(new GridLayout(3, false));

			final var lblSearchItems = new Label(groupSearch, SWT.NONE);
			lblSearchItems.setText("Search items");

			final var txtSearchInput = new Text(groupSearch, SWT.BORDER | SWT.READ_ONLY);
			txtSearchInput.setLayoutData(new GridData(195, SWT.DEFAULT));
			txtSearchInput.addMouseListener(mouseDoubleClickAdapter);

			final var lblPerformSearch = new Label(groupSearch, SWT.NONE);
			lblPerformSearch.setImage(CodeCadenzaResourcePlugin.getImage(IMG_SEARCH));

			addMenu(txtSearchInput);
		}

		final var lblAllItems = new Label(panList, SWT.NONE);
		lblAllItems.setText("Available items:");

		new Label(panList, SWT.NONE);

		final var lblSelItems = new Label(panList, SWT.NONE);
		lblSelItems.setText("Selected items");

		final var listAllItems = new List(panList, SWT.V_SCROLL | SWT.MULTI | SWT.H_SCROLL | SWT.BORDER);
		listAllItems.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		listAllItems.addMouseListener(mouseDoubleClickAdapter);

		final var panButtons = new Composite(panList, SWT.NONE);
		panButtons.setLayout(new GridLayout());

		final var cmdSelect = new Button(panButtons, SWT.NONE);
		cmdSelect.setImage(CodeCadenzaResourcePlugin.getImage(IMG_ARROW_RIGHT));
		cmdSelect.setSize(32, 32);

		final var cmdDeselect = new Button(panButtons, SWT.NONE);
		cmdDeselect.setImage(CodeCadenzaResourcePlugin.getImage(IMG_ARROW_LEFT));

		final var listSelectedItems = new List(panList, SWT.V_SCROLL | SWT.MULTI | SWT.H_SCROLL | SWT.BORDER);
		listSelectedItems.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		listSelectedItems.addMouseListener(mouseDoubleClickAdapter);

		if (columnSpan > 0)
			panList.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, columnSpan, 1));
		else
			panList.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		if (!span && formField.getColIndex() == 1)
			fillEmptyColumns();

		addMenu(listAllItems);
		addMenu(listSelectedItems);

		// Register listeners for moving the field by using one of the provided labels
		new FormFieldDragSourceListener(lblAllItems, formField);
		new FormFieldDragSourceListener(lblSelItems, formField);
		new FormFieldDropTargetListener(lblAllItems, formField, previewBuilder);
		new FormFieldDropTargetListener(lblSelItems, formField, previewBuilder);

		// Add a tool tip
		addToolTipText(listSelectedItems);
	}

}
