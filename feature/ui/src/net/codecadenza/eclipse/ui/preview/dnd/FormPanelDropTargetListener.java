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
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.DropTarget;
import org.eclipse.swt.dnd.DropTargetAdapter;
import org.eclipse.swt.dnd.DropTargetEvent;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.widgets.Composite;

/**
 * <p>
 * Drop target listener for moving a form field to another form panel in preview mode
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class FormPanelDropTargetListener extends DropTargetAdapter {
	private final VisualFormEditorPreviewBuilder formPreviewBuilder;
	private final FormPanel targetPanel;

	/**
	 * Constructor
	 * @param panTarget
	 * @param targetPanel
	 * @param formPreviewBuilder
	 */
	public FormPanelDropTargetListener(Composite panTarget, FormPanel targetPanel,
			VisualFormEditorPreviewBuilder formPreviewBuilder) {
		this.targetPanel = targetPanel;
		this.formPreviewBuilder = formPreviewBuilder;

		final var target = new DropTarget(panTarget, DND.DROP_MOVE | DND.DROP_COPY);
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

		moveFormFieldToPanel(draggedFormField);

		// Notify all listeners that the preview should be refreshed
		formPreviewBuilder.firePreviewChangeEvent();
	}

	/**
	 * @param draggedFormField
	 */
	private void moveFormFieldToPanel(final FormField draggedFormField) {
		final int fieldIndex = !targetPanel.getFields().isEmpty() ? targetPanel.getFields().size() - 1 : -1;

		// We simply place the dragged field at the end of the panel!
		draggedFormField.getPanel().getFields().remove(draggedFormField);
		targetPanel.getFields().add(draggedFormField);

		if (fieldIndex > 0)
			draggedFormField.setRowIndex(targetPanel.getFields().get(fieldIndex).getRowIndex() + 1);
		else
			draggedFormField.setRowIndex(1);

		draggedFormField.setColIndex(1);
	}

	/**
	 * @param event
	 * @return the dragged form field
	 */
	private FormField getDraggedFormField(DropTargetEvent event) {
		for (final FormField formField : targetPanel.getForm().getAllFormFields())
			if (formField.getName().equals(event.data)) {
				// The target panel and the panel of the form field must be different!
				if (formField.getPanel().equals(targetPanel))
					return null;

				return formField;
			}

		return null;
	}

}
