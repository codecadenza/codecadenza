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
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.service.form.FormService;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.eclipse.ui.dialog.client.CreateDefaultFormsDialog;
import net.codecadenza.eclipse.ui.dialog.client.EditFormGroupDialog;
import net.codecadenza.eclipse.ui.dialog.client.EditGridPanelDialog;
import net.codecadenza.eclipse.ui.dialog.client.EditTreeViewDialog;
import net.codecadenza.eclipse.ui.dialog.client.EditUpdateFormDialog;
import net.codecadenza.eclipse.ui.dialog.client.EditViewFormDialog;
import net.codecadenza.eclipse.ui.operation.DefaultFormBuildOperation;
import net.codecadenza.eclipse.ui.view.ProjectView;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

/**
 * <p>
 * Helper class to create a context menu for tree view items that represent form groups
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class FormGroupMenuBuilder extends AbstractMenuBuilder<FormGroup> {
	/**
	 * Constructor
	 * @param view
	 * @param tree
	 */
	public FormGroupMenuBuilder(ProjectView view, Tree tree) {
		super(view, tree);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.view.menu.AbstractMenuBuilder#createMenu()
	 */
	@Override
	public Menu createMenu() {
		final var menuGroup = new Menu(tree);

		// Add a menu item to add a new form group to an existing parent form group
		final var itemAdd = new MenuItem(menuGroup, SWT.NONE);
		itemAdd.setText(TITLE_ADD);

		itemAdd.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (EclipseIDEService.openDiagramExists(view))
					return;

				final FormGroup group = getSelectedObject();
				final var dlg = new EditFormGroupDialog(shell, group, true);

				if (dlg.open() != Dialog.OK)
					return;

				final var groupItem = new TreeItem(getSelectedItem(), SWT.NONE);
				groupItem.setText(dlg.getFormGroup().getName());
				groupItem.setData(dlg.getFormGroup());
				groupItem.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));
			}
		});

		// Add a menu item to create default forms and grid panels
		final var itemDefaultForms = new MenuItem(menuGroup, SWT.NONE);
		itemDefaultForms.setText(TITLE_CREATE_FORMS);

		itemDefaultForms.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (EclipseIDEService.openDiagramExists(view))
					return;

				final FormGroup group = getSelectedObject();
				final Project project = group.findProject();
				final var dlg = new CreateDefaultFormsDialog(shell, project);

				if (dlg.open() == Dialog.OK) {
					final IWorkbenchWindow workbenchWindow = view.getViewSite().getWorkbenchWindow();

					try {
						workbenchWindow.run(true, true, new DefaultFormBuildOperation(group, dlg.getConfiguration()));
					}
					catch (final InterruptedException ex) {
						Thread.currentThread().interrupt();
						CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
					}
					catch (final Exception ex) {
						CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
					}
				}

				view.refreshTree();
			}
		});

		// Add a menu item to create a new view form
		final var itemForm = new MenuItem(menuGroup, SWT.NONE);
		itemForm.setText(TITLE_CREATE_VIEW);

		itemForm.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (EclipseIDEService.openDiagramExists(view))
					return;

				new EditViewFormDialog(shell, getSelectedObject()).open();

				view.refreshTree();
			}
		});

		// Add a menu item to create a new grid panel
		final var itemGrid = new MenuItem(menuGroup, SWT.NONE);
		itemGrid.setText(TITLE_CREATE_GRID);

		itemGrid.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (EclipseIDEService.openDiagramExists(view))
					return;

				new EditGridPanelDialog(shell, getSelectedObject()).open();

				view.refreshTree();
			}
		});

		// Add a menu item to create a new update form
		final var itemNewUpdateForm = new MenuItem(menuGroup, SWT.NONE);
		itemNewUpdateForm.setText(TITLE_CREATE_FORM);

		itemNewUpdateForm.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (EclipseIDEService.openDiagramExists(view))
					return;

				final FormGroup group = getSelectedObject();
				final Project project = group.findProject();

				new EditUpdateFormDialog(shell, group, project).open();

				view.refreshTree();
			}
		});

		// Add a menu item to create a new tree view
		final var itemNewTreeView = new MenuItem(menuGroup, SWT.NONE);
		itemNewTreeView.setText(TITLE_CREATE_TREE);

		itemNewTreeView.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (EclipseIDEService.openDiagramExists(view))
					return;

				final FormGroup group = getSelectedObject();
				final Project project = group.findProject();

				new EditTreeViewDialog(shell, group, project).open();

				view.refreshTree();
			}
		});

		// Add a menu item to edit a form group
		final var itemEdit = new MenuItem(menuGroup, SWT.NONE);
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

				final FormGroup group = getSelectedObject();
				final var dlg = new EditFormGroupDialog(view.getSite().getShell(), group);

				if (dlg.open() != Dialog.OK)
					return;

				getSelectedItem().setText(dlg.getFormGroup().getName());
				getSelectedItem().setData(dlg.getFormGroup());
			}
		});

		// Add a menu item to rebuild all forms of the selected form group
		final var itemRebuildForms = new MenuItem(menuGroup, SWT.NONE);
		itemRebuildForms.setText(TITLE_REBUILD_ALL);

		itemRebuildForms.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final FormGroup group = getSelectedObject();
				final Project project = group.findProject();

				try {
					view.getViewSite().getWorkbenchWindow().run(true, false, monitor -> {
						try {
							final var formService = new FormService(project);

							int workStepCount = 0;

							monitor.beginTask("Rebuild forms...", group.getForms().size());

							for (final Form form : group.getForms()) {
								monitor.subTask("Rebuild " + form.getName());
								monitor.worked(workStepCount++);

								formService.rebuildForm(form);
							}

							workStepCount = 0;

							monitor.beginTask("Rebuild grid panels...", group.getPanels().size());

							for (final FormPanel gridPanel : group.getPanels()) {
								monitor.subTask("Rebuild " + gridPanel.getName());
								monitor.worked(workStepCount++);

								formService.rebuildGridPanel(gridPanel);
							}
						}
						catch (final Exception ex) {
							shell.getDisplay().syncExec(() -> CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex));
						}
						finally {
							monitor.done();
						}
					});
				}
				catch (final InterruptedException ex) {
					Thread.currentThread().interrupt();
					CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
				}
				catch (final Exception ex) {
					CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
				}
			}
		});

		// Add a menu item to delete the selected form group
		final var itemDelete = new MenuItem(menuGroup, SWT.NONE);
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

				final FormGroup group = getSelectedObject();

				if (group.getParentGroup() == null && group.getProject() == null)
					return;

				// Check if a group contains sub-groups
				if (!group.getChildGroups().isEmpty()) {
					MessageDialog.openInformation(shell, TITLE_DELETE, "The form group cannot be deleted as it contains sub-groups!");
					return;
				}

				// Check if a group contains forms or grid panels
				if (!group.getForms().isEmpty() || !group.getPanels().isEmpty()) {
					MessageDialog.openInformation(shell, TITLE_DELETE,
							"The form group cannot be deleted as long as it contains forms or grid panels!");
					return;
				}

				if (!MessageDialog.openConfirm(shell, TITLE_DELETE, "Do you really want to delete the selected form group?"))
					return;

				try {
					final Project project = group.findProject();

					if (group.getParentGroup() != null) {
						final FormGroup parentGroup = group.getParentGroup();
						parentGroup.getChildGroups().remove(group);
					}
					else {
						project.getFormGroups().remove(group);
						project.eResource().getContents().remove(group);
					}

					EclipseIDEService.saveProjectMetaData(project);

					view.refreshTree();
				}
				catch (final Exception ex) {
					CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
				}
			}
		});

		return menuGroup;
	}

}
