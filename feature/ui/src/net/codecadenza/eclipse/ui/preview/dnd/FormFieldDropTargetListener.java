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
package net.codecadenza.eclipse.ui.preview.dnd;

import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.ui.preview.VisualFormEditorPreviewBuilder;
import org.eclipse.swt.SWT;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.DropTarget;
import org.eclipse.swt.dnd.DropTargetAdapter;
import org.eclipse.swt.dnd.DropTargetEvent;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;

/**
 * <p>
 * Drop target listener for changing the position of form fields in preview mode
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class FormFieldDropTargetListener extends DropTargetAdapter {
	private static final int DEFAULT_WIDTH = 150;
	private final VisualFormEditorPreviewBuilder formPreviewBuilder;
	private final Color controlBackground;
	private FormField dropTargetField;
	private final Control dropTargetControl;
	private int dropRowIndex;
	private int dropColIndex;
	private FormPanel formPanel;

	/**
	 * Constructor for controls that are mapped to a form field
	 * @param dropTargetControl
	 * @param formField
	 * @param formPreviewBuilder
	 */
	public FormFieldDropTargetListener(Control dropTargetControl, FormField formField,
			VisualFormEditorPreviewBuilder formPreviewBuilder) {
		this(dropTargetControl, formField.getPanel(), formField.getRowIndex(), formField.getColIndex(), formPreviewBuilder);

		this.dropTargetField = formField;
		this.formPanel = formField.getPanel();
	}

	/**
	 * Constructor for controls that are responsible to fill empty columns in a grid layout
	 * @param dropTargetControl
	 * @param formPanel
	 * @param rowIndex
	 * @param columnIndex
	 * @param formPreviewBuilder
	 */
	public FormFieldDropTargetListener(Control dropTargetControl, FormPanel formPanel, int rowIndex, int columnIndex,
			VisualFormEditorPreviewBuilder formPreviewBuilder) {
		this.formPanel = formPanel;
		this.dropRowIndex = rowIndex;
		this.dropColIndex = columnIndex;
		this.formPreviewBuilder = formPreviewBuilder;
		this.dropTargetControl = dropTargetControl;
		this.controlBackground = dropTargetControl.getBackground();

		final var target = new DropTarget(dropTargetControl, DND.DROP_MOVE | DND.DROP_COPY);
		target.setTransfer(TextTransfer.getInstance());
		target.addDropListener(this);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.swt.dnd.DropTargetAdapter#drop(org.eclipse.swt.dnd.DropTargetEvent)
	 */
	@Override
	public void drop(DropTargetEvent event) {
		final FormField draggedFormField = getDraggedFormField(event);

		if (draggedFormField == null)
			return;

		// In case of identical fields we don't have to proceed!
		if (draggedFormField.equals(dropTargetField))
			return;

		moveFormField(draggedFormField, dropTargetField);

		// Notify all listeners that the preview should be refreshed
		formPreviewBuilder.firePreviewChangeEvent();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.swt.dnd.DropTargetAdapter#dragOver(org.eclipse.swt.dnd.DropTargetEvent)
	 */
	@Override
	public void dragOver(DropTargetEvent event) {
		dropTargetControl.setBackground(Display.getCurrent().getSystemColor(SWT.COLOR_DARK_YELLOW));
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.swt.dnd.DropTargetAdapter#dragLeave(org.eclipse.swt.dnd.DropTargetEvent)
	 */
	@Override
	public void dragLeave(DropTargetEvent event) {
		dropTargetControl.setBackground(controlBackground);
	}

	/**
	 * Exchange the form field positions in the same panel
	 * @param sourceField
	 * @param targetField
	 */
	private void moveFormField(final FormField sourceField, final FormField targetField) {
		// If no target field is selected the dragged field should be placed on an invisible label!
		if (targetField != null) {
			dropRowIndex = targetField.getRowIndex();
			dropColIndex = targetField.getColIndex();

			final boolean spanTargetField = targetField.isSpanCols() && sourceField.getColIndex() == 1;

			if (targetField.isSpanCols() && !spanTargetField)
				targetField.setWidth(DEFAULT_WIDTH);

			// Exchange the two fields in the grid layout
			targetField.setRowIndex(sourceField.getRowIndex());
			targetField.setColIndex(sourceField.getColIndex());
			targetField.setSpanCols(spanTargetField);
		}

		final boolean spanSourceField = sourceField.isSpanCols() && dropColIndex == 1;

		if (sourceField.isSpanCols() && !spanSourceField)
			sourceField.setWidth(DEFAULT_WIDTH);

		sourceField.setRowIndex(dropRowIndex);
		sourceField.setColIndex(dropColIndex);
		sourceField.setSpanCols(spanSourceField);
	}

	/**
	 * @param event
	 * @return the dragged form field
	 */
	private FormField getDraggedFormField(DropTargetEvent event) {
		for (final FormField formField : formPanel.getFields())
			if (formField.getName().equals(event.data)) {
				if (formField.equals(dropTargetField))
					return null;

				return formField;
			}

		return null;
	}

}
