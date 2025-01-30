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
package net.codecadenza.eclipse.ui.preview;

import net.codecadenza.eclipse.model.client.ClientFactory;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormAction;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.FormTable;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.client.TableColumnField;
import net.codecadenza.eclipse.service.form.init.UpdateFormInitService;
import net.codecadenza.eclipse.service.testing.gui.GUITestCaseService;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.eclipse.ui.dialog.client.EditActionDialog;
import net.codecadenza.eclipse.ui.dialog.client.EditFormFieldDialog;
import net.codecadenza.eclipse.ui.dialog.client.EditFormPanelDialog;
import net.codecadenza.eclipse.ui.dialog.client.EditTableColumnFieldDialog;
import net.codecadenza.eclipse.ui.dialog.client.GridPanelSelectDialog;
import net.codecadenza.eclipse.ui.dialog.client.NewFormFieldDialog;
import net.codecadenza.eclipse.ui.preview.dnd.FormPanelDropTargetListener;
import net.codecadenza.eclipse.ui.preview.event.PreviewChangeController;
import net.codecadenza.eclipse.ui.preview.field.FieldPreviewBuilderFactory;
import net.codecadenza.eclipse.ui.preview.field.imp.AbstractVisualEditorFieldPreviewBuilder;
import net.codecadenza.eclipse.ui.preview.util.TableColumnResizeListener;
import net.codecadenza.eclipse.ui.preview.util.TableColumnSelectionListener;
import net.codecadenza.runtime.richclient.eclipse.widget.DataGridComposite;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;

/**
 * <p>
 * Preview builder for the visual form editor
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class VisualFormEditorPreviewBuilder extends AbstractFormPreviewBuilder {
	private static final String PANEL_PREFIX = "pan";

	private final PreviewChangeController previewChangeController;
	private boolean editMode = true;
	private UpdateFormInitService initService;
	private FormPanel selectedPanel;

	/**
	 * Constructor
	 * @param panParent
	 * @param previewChangeController
	 */
	public VisualFormEditorPreviewBuilder(Composite panParent, PreviewChangeController previewChangeController) {
		super(panParent);

		this.previewChangeController = previewChangeController;
	}

	/**
	 * @param initService
	 */
	public void setInitService(UpdateFormInitService initService) {
		this.initService = initService;
	}

	/**
	 * @param editMode
	 */
	public void setEditMode(boolean editMode) {
		this.editMode = editMode;
	}

	/**
	 * Notify listeners that the preview should be refreshed
	 */
	public void firePreviewChangeEvent() {
		previewChangeController.fireChangeEvent();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.preview.AbstractFormPreviewBuilder#addGridPanelMenuItems(org.eclipse.swt.widgets.Menu,
	 * net.codecadenza.runtime.richclient.eclipse.widget.DataGridComposite, net.codecadenza.eclipse.model.client.FormPanel)
	 */
	@Override
	protected void addGridPanelMenuItems(Menu gridPanelMenu, DataGridComposite<?> dataGrid, FormPanel gridPanel) {
		final FormTable formTable = gridPanel != null ? gridPanel.getFormTable() : form.getFormPanels().get(0).getFormTable();
		final boolean addCreateActionItem = gridPanel != null || formType != FormTypeEnumeration.LOV;

		if (addCreateActionItem) {
			// Add a menu item to create a new action
			final var mniCreateAction = new MenuItem(gridPanelMenu, SWT.NONE);
			mniCreateAction.setText("Create new action");

			mniCreateAction.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(SelectionEvent e) {
					final EditActionDialog dlg;

					if (form != null)
						dlg = new EditActionDialog(shell, form, project.getRoles());
					else
						dlg = new EditActionDialog(shell, gridPanel, project.getRoles());

					dlg.open();

					if (dlg.getReturnCode() == Dialog.OK)
						previewChangeController.fireChangeEvent();
				}
			});
		}

		// Register listeners
		for (final TableColumn tableColumn : dataGrid.getTableViewer().getTable().getColumns()) {
			tableColumn.setMoveable(true);

			if (gridPanel != null) {
				tableColumn.addListener(SWT.Selection, new TableColumnSelectionListener(gridPanel, previewChangeController));
				tableColumn.addControlListener(new TableColumnResizeListener(gridPanel));
			}
			else {
				tableColumn.addListener(SWT.Selection, new TableColumnSelectionListener(form, previewChangeController));
				tableColumn.addControlListener(new TableColumnResizeListener(form));
			}
		}

		// Add the menu items for invisible columns
		for (final TableColumnField col : formTable.getFields()) {
			if (col.isVisible())
				continue;

			final var mniEdit = new MenuItem(gridPanelMenu, SWT.NONE);
			mniEdit.setText("Edit column '" + col.getTitle() + "'");

			mniEdit.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(SelectionEvent e) {
					final var dlg = new EditTableColumnFieldDialog(shell, project, col, formType);

					if (dlg.open() != Dialog.CANCEL)
						previewChangeController.fireChangeEvent();
				}
			});
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.preview.AbstractFormPreviewBuilder#addPanelToForm(org.eclipse.swt.widgets.Composite,
	 * net.codecadenza.eclipse.model.client.FormPanel, boolean)
	 */
	@Override
	protected Composite addPanelToForm(Composite panParent, FormPanel formPanel, boolean grabVerticalSpace) {
		final Composite panContent = super.addPanelToForm(panParent, formPanel, grabVerticalSpace);

		new FormPanelDropTargetListener(panContent, formPanel, this);

		return panContent;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.preview.AbstractFormPreviewBuilder#addFormFields(org.eclipse.swt.widgets.Composite,
	 * net.codecadenza.eclipse.model.client.FormPanel)
	 */
	@Override
	protected void addFormFields(Composite panParent, FormPanel formPanel) {
		// Rearrange all fields in order to avoid display problems because of inappropriate layout settings
		formPanel.rearrangeFields();

		formPanel.getFields().forEach(field -> {
			if (field.isHidden()) {
				// Add a menu item to edit an invisible form field
				final var mniEdit = new MenuItem(panParent.getMenu(), SWT.NONE);
				mniEdit.setText("Edit field '" + field.getLabel() + "'");

				mniEdit.addSelectionListener(new SelectionAdapter() {
					/*
					 * (non-Javadoc)
					 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
					 */
					@Override
					public void widgetSelected(SelectionEvent e) {
						final var dlg = new EditFormFieldDialog(shell, project, field, formType);

						if (dlg.open() == Dialog.OK) {
							// Notify all listeners that the preview should be refreshed
							firePreviewChangeEvent();
						}
					}
				});
			}
			else
				FieldPreviewBuilderFactory.getFieldBuilder(this, field, panParent).generateFieldPreview();
		});

		// If the current form panel and the selected panel are equal an additional row will be added
		if (formPanel.equals(selectedPanel)) {
			AbstractVisualEditorFieldPreviewBuilder.addFillLabel(panParent, formPanel, this, emptyRowIndex, 1);
			AbstractVisualEditorFieldPreviewBuilder.addFillLabel(panParent, formPanel, this, emptyRowIndex, 2);

			// As soon as the preview is refreshed the empty row should disappear again!
			selectedPanel = null;
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.preview.AbstractFormPreviewBuilder#addFormPanelMenuItems(org.eclipse.swt.widgets.Menu,
	 * net.codecadenza.eclipse.model.client.FormPanel, boolean)
	 */
	@Override
	protected void addFormPanelMenuItems(Menu mnuPanel, FormPanel formPanel, boolean hasGrid) {
		final var itemEditText = "Edit panel";
		final var itemAddField = "Add field";
		final var itemAddRow = "Add row";
		final var itemCreateText = "Create new panel";
		final var itemAddGridPanelText = "Add grid panel";
		final var itemDeleteText = "Delete panel";

		final var mniEdit = new MenuItem(mnuPanel, SWT.NONE);
		mniEdit.setText(itemEditText);

		mniEdit.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final var dlg = new EditFormPanelDialog(shell, formPanel);

				if (dlg.open() == Dialog.OK)
					previewChangeController.fireChangeEvent();
			}
		});

		if (!hasGrid) {
			if (initService != null) {
				final var mniAddField = new MenuItem(mnuPanel, SWT.NONE);
				mniAddField.setText(itemAddField);

				mniAddField.addSelectionListener(new SelectionAdapter() {
					/*
					 * (non-Javadoc)
					 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
					 */
					@Override
					public void widgetSelected(SelectionEvent e) {
						final var dlg = new NewFormFieldDialog(shell, formPanel, initService, editMode);

						if (dlg.open() == Dialog.OK) {
							// Notify all listeners that the preview should be refreshed
							firePreviewChangeEvent();
						}
					}
				});
			}

			final var mniAddRow = new MenuItem(mnuPanel, SWT.NONE);
			mniAddRow.setText(itemAddRow);

			mniAddRow.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(SelectionEvent e) {
					int lastRowIndex = 0;

					for (final FormField field : formPanel.getFields()) {
						if (field.isHidden())
							continue;

						lastRowIndex = field.getRowIndex();
					}

					emptyRowIndex = lastRowIndex + 1;
					selectedPanel = formPanel;

					// Refresh the preview in order to show the additional row
					firePreviewChangeEvent();
				}
			});
		}

		final var mniCreate = new MenuItem(mnuPanel, SWT.NONE);
		mniCreate.setText(itemCreateText);

		mniCreate.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final var dlg = new EditFormPanelDialog(shell);
				dlg.open();

				if (dlg.getReturnCode() != Dialog.OK)
					return;

				final Form form = formPanel.getForm();

				final FormPanel panel = dlg.getPanel();
				panel.setForm(form);

				form.getFormPanels().add(panel);

				if (editMode)
					for (final FormPanel p : form.getFormPanels())
						if (!project.eResource().getContents().contains(p))
							project.eResource().getContents().add(p);

				previewChangeController.fireChangeEvent();
			}
		});

		if (editMode && formType != FormTypeEnumeration.ADD && formType != FormTypeEnumeration.CREATE) {
			final var mniAddGridPanel = new MenuItem(mnuPanel, SWT.NONE);
			mniAddGridPanel.setText(itemAddGridPanelText);

			mniAddGridPanel.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(SelectionEvent e) {
					final Form form = formPanel.getForm();

					final var dlg = new GridPanelSelectDialog(shell, form);
					dlg.open();

					if (dlg.getReturnCode() != Dialog.OK)
						return;

					final FormPanel basePanel = dlg.getSelectedFormPanel();
					int colIndex = 0;

					if (basePanel == null)
						return;

					// Calculate the next column index
					for (final FormPanel p : form.getFormPanels())
						if (p.getRowIndex() == 2 && p.getColIndex() > colIndex)
							colIndex = p.getColIndex();

					final String panelName = basePanel.getAssociation().getUpperCaseName();

					// Create a default label for this panel
					final var b = new StringBuilder();

					for (final char c : panelName.toCharArray())
						if (c == Character.toUpperCase(c))
							b.append(" " + Character.toLowerCase(c));
						else
							b.append(c);

					String panelLabel = b.toString().trim();
					panelLabel = panelLabel.substring(0, 1).toUpperCase() + panelLabel.substring(1);

					final FormPanel newPanel = ClientFactory.eINSTANCE.createFormPanel();
					newPanel.setBasePanel(basePanel);
					newPanel.setColIndex(colIndex + 1);
					newPanel.setRowIndex(2);
					newPanel.setName(PANEL_PREFIX + panelName);
					newPanel.setLabel(panelLabel);
					newPanel.setForm(form);

					form.getFormPanels().add(newPanel);

					if (!project.eResource().getContents().contains(newPanel))
						project.eResource().getContents().add(newPanel);

					previewChangeController.fireChangeEvent();
				}
			});
		}

		final var mniDelete = new MenuItem(mnuPanel, SWT.NONE);
		mniDelete.setText(itemDeleteText);
		mniDelete.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_TOOL_DELETE));

		mniDelete.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (formPanel.getBasePanel() == null && !formPanel.getFields().isEmpty()) {
					MessageDialog.openInformation(shell, itemDeleteText, "The panel cannot be deleted as long as it contains form fields!");
					return;
				}

				final boolean doIt = MessageDialog.openQuestion(shell, itemDeleteText,
						"Do you really want to delete the selected panel?");

				if (!doIt)
					return;

				try {
					// Synchronize GUI tests when removing a grid panel
					if (editMode && formPanel.getBasePanel() != null)
						new GUITestCaseService(project).syncOnRemoveGridPanel(formPanel);
				}
				catch (final Exception ex) {
					CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
				}

				formPanel.getForm().getFormPanels().remove(formPanel);
				project.eResource().getContents().remove(formPanel);

				previewChangeController.fireChangeEvent();
			}
		});
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.preview.AbstractFormPreviewBuilder#addToolBarItemSelectionListener(org.eclipse.swt.widgets.
	 * ToolItem, net.codecadenza.eclipse.model.client.FormAction, boolean)
	 */
	@Override
	protected void addToolBarItemSelectionListener(ToolItem toolItem, FormAction action, boolean editAction) {
		if (!editAction)
			return;

		toolItem.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final var guiTestCaseService = new GUITestCaseService(project);
				final var dlg = new EditActionDialog(shell, project.getRoles(), action);
				final int returnCode = dlg.open();

				if (returnCode == EditActionDialog.DELETE) {
					final boolean doIt = MessageDialog.openQuestion(shell, "Delete action",
							"Do you really want to delete the selected action?");

					if (!doIt)
						return;

					if (editMode)
						try {
							// Synchronize GUI tests
							if (action.getForm() != null)
								guiTestCaseService.syncOnDeleteFormAction(action.getForm(), action);

							if (action.getPanel() != null)
								guiTestCaseService.syncOnDeleteFormAction(action);
						}
						catch (final Exception ex) {
							CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
						}

					if (action.getForm() != null)
						action.getForm().getActions().remove(action);

					if (action.getPanel() != null)
						action.getPanel().getActions().remove(action);

					previewChangeController.fireChangeEvent();
				}
				else if (returnCode == Dialog.OK)
					previewChangeController.fireChangeEvent();
			}
		});
	}

}
