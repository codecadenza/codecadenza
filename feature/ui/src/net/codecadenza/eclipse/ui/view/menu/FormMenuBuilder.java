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
package net.codecadenza.eclipse.ui.view.menu;

import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormGroup;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.client.TreeView;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.service.boundary.BoundaryService;
import net.codecadenza.eclipse.service.dto.DTOBeanService;
import net.codecadenza.eclipse.service.form.FormService;
import net.codecadenza.eclipse.service.testing.gui.GUITestCaseService;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.eclipse.ui.dialog.client.EditTreeViewDialog;
import net.codecadenza.eclipse.ui.dialog.client.EditUpdateFormDialog;
import net.codecadenza.eclipse.ui.dialog.client.EditViewFormDialog;
import net.codecadenza.eclipse.ui.preview.VisualFormEditorDialog;
import net.codecadenza.eclipse.ui.preview.VisualFormEditorTitleAreaDialog;
import net.codecadenza.eclipse.ui.view.ProjectView;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;

/**
 * <p>
 * Helper class to create a context menu for tree view items that represent forms
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class FormMenuBuilder extends AbstractMenuBuilder<Form> {
	/**
	 * Constructor
	 * @param view
	 * @param tree
	 */
	public FormMenuBuilder(ProjectView view, Tree tree) {
		super(view, tree);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.view.menu.AbstractMenuBuilder#createMenu()
	 */
	@Override
	public Menu createMenu() {
		final var menuForm = new Menu(tree);

		// Add a menu item to open the form source file
		final var itemOpen = new MenuItem(menuForm, SWT.NONE);
		itemOpen.setText(TITLE_OPEN);

		itemOpen.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				view.openFormInEditor(getSelectedItem());
			}
		});

		// Add a menu item to edit an existing form
		final var itemEdit = new MenuItem(menuForm, SWT.NONE);
		itemEdit.setText(TITLE_EDIT);

		itemEdit.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (EclipseIDEService.openDiagramExists(view))
					return;

				final Form form = getSelectedObject();
				final FormGroup group = form.getFormGroup();
				final Project project = group.findProject();
				final Dialog dlg;

				if (form.getFormType() == FormTypeEnumeration.SIMPLE_VIEW || form.getFormType() == FormTypeEnumeration.SEARCHABLE_VIEW
						|| form.getFormType() == FormTypeEnumeration.LOV)
					dlg = new EditViewFormDialog(shell, group, form);
				else if (form.getFormType() == FormTypeEnumeration.TREE_VIEW)
					dlg = new EditTreeViewDialog(shell, (TreeView) form, project);
				else
					dlg = new EditUpdateFormDialog(shell, group, project, form);

				dlg.open();

				// Refresh the tree no matter if the dialog has been closed with 'Cancel' or 'OK' in order to revert or to sync changes!
				view.refreshTree();
			}
		});

		// Add a menu item to show a preview of the selected form
		final var itemPreview = new MenuItem(menuForm, SWT.NONE);
		itemPreview.setText(TITLE_PREVIEW);

		itemPreview.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (EclipseIDEService.openDiagramExists(view))
					return;

				final Form form = getSelectedObject();
				final FormTypeEnumeration formType = form.getFormType();
				final Project project = form.getDTO().getNamespace().getProject();
				final Dialog dlg;

				if (formType == FormTypeEnumeration.TREE_VIEW) {
					MessageDialog.openInformation(shell, TITLE_PREVIEW, "A preview for tree view forms is not supported!");
					return;
				}

				if (formType == FormTypeEnumeration.SIMPLE_VIEW || formType == FormTypeEnumeration.SEARCHABLE_VIEW
						|| formType == FormTypeEnumeration.LOV || form.isTitleArea())
					dlg = new VisualFormEditorTitleAreaDialog(shell, form);
				else
					dlg = new VisualFormEditorDialog(shell, form);

				if (dlg.open() == Dialog.OK)
					try {
						EclipseIDEService.saveProjectMetaData(project);

						// Rebuild the form
						new FormService(project).rebuildForm(form);

						// Rebuild the DTO
						new DTOBeanService(project).rebuildDTOBeanSourceFiles(form.getDTO());

						// Rebuild the boundary
						new BoundaryService(project).rebuildBoundarySourceFiles(project.getBoundaryByDomainObject(form.getDomainObject()));

						// Synchronize GUI tests
						new GUITestCaseService(project).syncOnEditForm(form);
					}
					catch (final Exception ex) {
						CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
					}

				// Refresh the tree no matter if the dialog has been closed with 'Cancel' or 'OK' in order to revert or to sync changes!
				view.refreshTree();
			}
		});

		// Add a menu item to rename the selected form
		final var itemRename = new MenuItem(menuForm, SWT.NONE);
		itemRename.setText(TITLE_RENAME);

		itemRename.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (EclipseIDEService.openDiagramExists(view))
					return;

				final Form form = getSelectedObject();
				final Project project = form.getDTO().getNamespace().getProject();

				try {
					final var dlg = new InputDialog(shell, TITLE_RENAME, "Enter a new form name:", form.getName(), null);

					if (dlg.open() != Window.OK)
						return;

					new FormService(project).renameForm(form, dlg.getValue());

					EclipseIDEService.saveProjectMetaData(project);

					getSelectedItem().setText(form.getName());
				}
				catch (final IllegalStateException ex) {
					MessageDialog.openInformation(shell, TITLE_RENAME, ex.getMessage());
				}
				catch (final Exception ex) {
					CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
				}
			}
		});

		// Add a menu item to rebuild the form source file
		final var itemRebuild = new MenuItem(menuForm, SWT.NONE);
		itemRebuild.setText(TITLE_REBUILD);

		itemRebuild.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final Form form = getSelectedObject();

				try {
					new FormService(form.getDomainObject().getNamespace().getProject()).rebuildForm(form);
				}
				catch (final Exception ex) {
					CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
				}
			}
		});

		// Add a menu item to synchronize actions
		final var itemSync = new MenuItem(menuForm, SWT.NONE);
		itemSync.setText(TITLE_SYNC_ACTIONS);

		itemSync.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (EclipseIDEService.openDiagramExists(view))
					return;

				final Form form = getSelectedObject();

				if (form.getFormType() != FormTypeEnumeration.SEARCHABLE_VIEW && form.getFormType() != FormTypeEnumeration.SIMPLE_VIEW) {
					MessageDialog.openInformation(shell, TITLE_SYNC_ACTIONS, "This operation is not supported for the selected form!");
					return;
				}

				try {
					final int numberOfNewActions = new FormService(form.getDomainObject().getNamespace().getProject()).syncActions(form);

					if (numberOfNewActions == 0)
						MessageDialog.openInformation(shell, TITLE_SYNC_ACTIONS, "No new actions found!");
					else
						MessageDialog.openInformation(shell, TITLE_SYNC_ACTIONS, "Number of new actions: " + numberOfNewActions);
				}
				catch (final Exception ex) {
					CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
				}
			}
		});

		// Add a menu item to delete the selected form
		final var itemDelete = new MenuItem(menuForm, SWT.NONE);
		itemDelete.setText(TITLE_DELETE);
		itemDelete.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_TOOL_DELETE));

		itemDelete.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (EclipseIDEService.openDiagramExists(view))
					return;

				if (!MessageDialog.openConfirm(shell, TITLE_DELETE, "Do you really want to delete the selected form?"))
					return;

				try {
					final Project project = getSelectedObject().getDomainObject().getNamespace().getProject();

					new FormService(project).deleteForm(getSelectedObject());

					view.refreshTree();
				}
				catch (final Exception ex) {
					CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
				}
			}
		});

		return menuForm;
	}

}
