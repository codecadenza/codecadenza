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
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.service.form.FormService;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.eclipse.ui.dialog.client.EditFormFieldDialog;
import net.codecadenza.eclipse.ui.preview.VisualFormEditorPreviewBuilder;
import net.codecadenza.eclipse.ui.preview.dnd.FormFieldDragSourceListener;
import net.codecadenza.eclipse.ui.preview.dnd.FormFieldDropTargetListener;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;

/**
 * <p>
 * Abstract preview builder for fields that are displayed in a visual form editor
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractVisualEditorFieldPreviewBuilder extends AbstractFieldPreviewBuilder {
	protected final VisualFormEditorPreviewBuilder previewBuilder;

	/**
	 * Constructor
	 * @param previewBuilder
	 * @param formField
	 * @param formPanel
	 */
	protected AbstractVisualEditorFieldPreviewBuilder(VisualFormEditorPreviewBuilder previewBuilder, FormField formField,
			Composite formPanel) {
		super(formField, formPanel);

		this.previewBuilder = previewBuilder;
	}

	/**
	 * Add a label to fill empty columns in the grid layout
	 * @param panParent
	 * @param formPanel
	 * @param previewBuilder
	 * @param rowIndex
	 * @param colIndex
	 */
	public static final void addFillLabel(Composite panParent, FormPanel formPanel, VisualFormEditorPreviewBuilder previewBuilder,
			int rowIndex, int colIndex) {
		final var lblFill = new Label(panParent, SWT.NONE);
		lblFill.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 2, 1));

		new FormFieldDropTargetListener(lblFill, formPanel, rowIndex, colIndex, previewBuilder);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.preview.field.imp.AbstractFieldPreviewBuilder#addFieldListeners(org.eclipse.swt.widgets.
	 * Control, org.eclipse.swt.widgets.Control)
	 */
	@Override
	protected void addFieldListeners(Control label, Control field) {
		// Register listeners for moving a form field by using the respective label
		new FormFieldDragSourceListener(label, formField);
		new FormFieldDropTargetListener(label, formField, previewBuilder);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.preview.field.imp.AbstractFieldPreviewBuilder#onDoubleClickField()
	 */
	@Override
	protected void onDoubleClickField() {
		editFormField();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.preview.field.imp.AbstractFieldPreviewBuilder#addMenu(org.eclipse.swt.widgets.Control)
	 */
	@Override
	protected void addMenu(Control field) {
		final var mnuControl = new Menu(field);

		// Add a menu item to edit the selected form field
		final var mniEdit = new MenuItem(mnuControl, SWT.NONE);
		mniEdit.setText("Edit");

		mniEdit.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				editFormField();
			}
		});

		// Add a menu item to hide the selected form field
		final var mniHide = new MenuItem(mnuControl, SWT.NONE);
		mniHide.setText("Hide");

		mniHide.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				formField.setVisible(false);

				// Notify all listeners that the preview should be refreshed
				previewBuilder.firePreviewChangeEvent();
			}
		});

		// Add a menu item to delete the selected form field
		final var mniDelete = new MenuItem(mnuControl, SWT.NONE);
		mniDelete.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_TOOL_DELETE));
		mniDelete.setText("Delete");

		mniDelete.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final DomainAttribute domainAttribute = formField.getDTOAttribute().getDomainAttribute();
				final var msgTitle = "Delete form field";
				var msgText = "Do you really want to delete the selected field?";

				// Check if the user has selected a primary key field that should be deleted!
				if (domainAttribute != null && domainAttribute.isPk())
					msgText += "\nThe field references an ID attribute. Some components won't work correctly if this attribute is missing!";

				final boolean doIt = MessageDialog.openQuestion(shell, msgTitle, msgText);

				if (!doIt)
					return;

				try {
					new FormService(project).deleteFormField(formField);
				}
				catch (final Exception ex) {
					CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
					return;
				}

				// Notify all listeners that the preview should be refreshed
				previewBuilder.firePreviewChangeEvent();
			}
		});

		field.setMenu(mnuControl);
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

		addFillLabel(formPanel, formField.getPanel(), previewBuilder, rowIndex, formField.getColIndex() == 1 ? 2 : 1);
	}

	/**
	 * Open the dialog to edit this form field
	 */
	private void editFormField() {
		final var dlg = new EditFormFieldDialog(shell, project, formField, formType);

		if (dlg.open() == Dialog.OK) {
			// Notify all listeners that the preview should be refreshed
			previewBuilder.firePreviewChangeEvent();
		}
	}

}
