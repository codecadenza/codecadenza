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
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.ui.preview.field.FieldPreviewBuilder;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Link;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

/**
 * <p>
 * Abstract base class for generating a preview of a given form field
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractFieldPreviewBuilder implements FieldPreviewBuilder {
	private static final int DEFAULT_MULTI_LINE_CONTROL_HEIGHT = 100;

	protected final FormField formField;
	protected final Composite formPanel;
	protected final Project project;
	protected final FormTypeEnumeration formType;
	protected final Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
	protected MouseDoubleClickAdapter mouseDoubleClickAdapter = new MouseDoubleClickAdapter();
	protected boolean setGridData = true;

	/**
	 * Constructor
	 * @param formField
	 * @param formPanel
	 */
	protected AbstractFieldPreviewBuilder(FormField formField, Composite formPanel) {
		this.formField = formField;
		this.formPanel = formPanel;
		this.formType = formField.getPanel().getForm().getFormType();
		this.project = formField.getPanel().getForm().getDomainObject().getNamespace().getProject();
	}

	/**
	 * Every derived class must define how to create a preview of a given form field
	 * @param lblField
	 * @return the control
	 */
	protected abstract Control getFieldPreview(Control lblField);

	/**
	 * Add labels in order to fill empty cells within the panel's grid layout
	 */
	protected abstract void fillEmptyColumns();

	/**
	 * Add a menu to a visible field. The method provides no implementation as it is up to the derived class to define the
	 * respective functionality!
	 * @param field
	 */
	@SuppressWarnings("unused")
	protected void addMenu(Control field) {

	}

	/**
	 * Add listeners to either a visible field, or its label. The method provides no implementation as it is up to the derived class
	 * to define the respective functionality!
	 * @param label
	 * @param field
	 */
	@SuppressWarnings("unused")
	protected void addFieldListeners(Control label, Control field) {

	}

	/**
	 * Callback listener for double-click events on a visible field. The method provides no implementation as it is up to the
	 * derived class to define the respective functionality!
	 */
	protected void onDoubleClickField() {

	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.preview.field.FieldPreviewBuilder#generateFieldPreview()
	 */
	@Override
	public void generateFieldPreview() {
		if (formField.getColIndex() == 2)
			fillEmptyColumns();

		final Control lblField = addFieldLabel();
		final Control control = getFieldPreview(lblField);

		if (formField.getColIndex() == 1)
			fillEmptyColumns();

		if (control != null) {
			control.addMouseListener(mouseDoubleClickAdapter);

			addFieldListeners(lblField, control);

			// For some fields (e.g. checkboxes) grid data must not be set!
			if (setGridData)
				setGridData(control);

			// Add a tool tip
			addToolTipText(control);

			// Add a menu
			addMenu(control);
		}
	}

	/**
	 * Adapter for double-click events
	 */
	private class MouseDoubleClickAdapter extends MouseAdapter {
		/*
		 * (non-Javadoc)
		 * @see org.eclipse.swt.events.MouseAdapter#mouseDoubleClick(org.eclipse.swt.events.MouseEvent)
		 */
		@Override
		public void mouseDoubleClick(MouseEvent e) {
			onDoubleClickField();
		}
	}

	/**
	 * @return the generated label for the given field
	 */
	protected Control addFieldLabel() {
		if (formField.isAddFormLinkToLabel()) {
			final var link = new Link(formPanel, SWT.NONE);
			link.setText("<a>" + formField.getLabel() + " :</a>");

			return link;
		}

		final var lblField = new Label(formPanel, SWT.NONE);
		lblField.setText(formField.getLabel() + " :");

		if (formField.isMandatory() && !formField.isReadonly())
			lblField.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_BLUE));

		return lblField;
	}

	/**
	 * @param control
	 */
	protected void addToolTipText(Control control) {
		final AbstractDomainAssociation assoc = formField.getDTOAttribute().getAssociation();
		final DomainAttribute attr = formField.getDTOAttribute().getDomainAttribute();

		if (attr != null && attr.getUserComment() != null && !attr.getUserComment().isEmpty())
			control.setToolTipText(attr.getUserComment());

		if (attr == null && assoc != null && assoc.getUserComment() != null && !assoc.getUserComment().isEmpty())
			control.setToolTipText(assoc.getUserComment());
	}

	/**
	 * Set the grid data for the given control
	 * @param control
	 */
	protected void setGridData(Control control) {
		final FormFieldTypeEnumeration fieldType = formField.getFieldType();
		final boolean allowSpan = formField.isSpanCols() && formField.getColIndex() == 1;
		final GridData gdControl;
		boolean setDefaultHeight = false;

		if (fieldType == FormFieldTypeEnumeration.MULTI_LINE_TEXT || fieldType == FormFieldTypeEnumeration.MULTI_LINE_LABEL)
			setDefaultHeight = true;

		if (allowSpan) {
			if (formField.getFieldType() == FormFieldTypeEnumeration.MULTI_LINE_TEXT)
				gdControl = new GridData(SWT.FILL, SWT.FILL, true, true, 3, 1);
			else
				gdControl = new GridData(SWT.FILL, SWT.CENTER, true, false, 3, 1);
		}
		else
			gdControl = new GridData(SWT.LEFT, SWT.CENTER, true, false);

		if (!allowSpan && formField.getWidth() > 0)
			gdControl.widthHint = formField.getWidth();

		if (setDefaultHeight)
			gdControl.heightHint = DEFAULT_MULTI_LINE_CONTROL_HEIGHT;

		control.setLayoutData(gdControl);
	}

	/**
	 * @return the current row index of an empty grid column. A value that is smaller or equal to 0 means that no empty column was
	 *         found!
	 */
	protected int getRowIndexOfEmptyColumn() {
		FormField nextField = null;
		FormField previousField = null;
		int rowIndex = 0;

		// Skip fields that really span over two columns!
		if (formField.isSpanCols() && formField.getColIndex() == 1)
			return -1;

		final int fieldIndex = formField.getPanel().getFields().indexOf(formField);

		// Determine if it is necessary to add a label in order to fill the panel's grid layout!
		if (formField.getColIndex() == 1) {
			// If the field is in the first column we must search for the next field!
			if ((fieldIndex + 1) < formField.getPanel().getFields().size()) {
				nextField = formField.getPanel().getFields().get(fieldIndex + 1);

				// An empty label must be added if both fields are placed on different rows
				if (nextField.getRowIndex() != formField.getRowIndex())
					rowIndex = formField.getRowIndex();
			}
			else {
				// If the current field is the last in the list we want to fill up the second column!
				rowIndex = formField.getRowIndex();
			}
		}
		else if (fieldIndex > 0) {
			// If the field is in the second column we must search for the previous field!
			previousField = formField.getPanel().getFields().get(fieldIndex - 1);

			// An empty label must be added if both fields are placed on different rows
			if (previousField.getRowIndex() != formField.getRowIndex())
				rowIndex = formField.getRowIndex();
		}
		else {
			// If the current field in the second column is the first in the list we must fill up the first column!
			rowIndex = formField.getRowIndex();
		}

		return rowIndex;
	}

}
