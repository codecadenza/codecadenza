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

import net.codecadenza.eclipse.model.client.FormGroup;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.service.boundary.BoundaryService;
import net.codecadenza.eclipse.service.dto.DTOBeanService;
import net.codecadenza.eclipse.service.form.FormService;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.eclipse.ui.dialog.client.EditGridPanelDialog;
import net.codecadenza.eclipse.ui.preview.VisualFormEditorDialog;
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
 * Helper class to create a context menu for tree view items that represent form panels
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class PanelMenuBuilder extends AbstractMenuBuilder<FormPanel> {
	/**
	 * Constructor
	 * @param view
	 * @param tree
	 */
	public PanelMenuBuilder(ProjectView view, Tree tree) {
		super(view, tree);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.view.menu.AbstractMenuBuilder#createMenu()
	 */
	@Override
	public Menu createMenu() {
		final var menuForm = new Menu(tree);

		// Add a menu item to open the grid panel source file
		final var itemOpen = new MenuItem(menuForm, SWT.NONE);
		itemOpen.setText(TITLE_OPEN);

		itemOpen.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				view.openGridPanelInEditor(getSelectedItem());
			}
		});

		// Add a menu item to edit a grid panel
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

				final FormPanel panel = getSelectedObject();
				final FormGroup group = panel.getFormGroup();

				new EditGridPanelDialog(shell, group, panel).open();

				view.refreshTree();
			}
		});

		// Add a menu item to show a preview
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

				final FormPanel panel = getSelectedObject();
				final Project project = panel.getDTO().getNamespace().getProject();

				// Show the grid panel in the preview dialog
				final var dlg = new VisualFormEditorDialog(shell, panel);

				if (dlg.open() != Dialog.OK)
					return;

				try {
					EclipseIDEService.saveProjectMetaData(project);

					// Rebuild the grid panel
					new FormService(project).rebuildGridPanel(panel);

					// Rebuild the DTO
					new DTOBeanService(project).rebuildDTOBeanSourceFiles(panel.getDTO());

					// Rebuild the boundary
					new BoundaryService(project)
							.rebuildBoundarySourceFiles(project.getBoundaryByDomainObject(panel.getDTO().getDomainObject()));
				}
				catch (final Exception ex) {
					CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
				}

				// Refresh the tree no matter if the dialog has been closed with 'Cancel' or 'OK' in order to revert or to sync changes!
				view.refreshTree();
			}
		});

		// Add a menu item to rename the selected grid panel
		final var itemRename = new MenuItem(menuForm, SWT.NONE);
		itemRename.setText(TITLE_RENAME);

		itemRename.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (EclipseIDEService.openDiagramExists(view))
					return;

				final FormPanel panel = getSelectedObject();
				final Project project = panel.getDTO().getNamespace().getProject();

				try {
					final var dlg = new InputDialog(shell, TITLE_RENAME, "Enter a new grid panel name:", panel.getName(), null);

					if (dlg.open() != Window.OK)
						return;

					new FormService(project).renameGridPanel(panel, dlg.getValue());

					EclipseIDEService.saveProjectMetaData(project);

					getSelectedItem().setText(panel.getName());
				}
				catch (final IllegalStateException ex) {
					MessageDialog.openInformation(shell, TITLE_RENAME, ex.getMessage());
				}
				catch (final Exception ex) {
					CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
				}
			}
		});

		// Add a menu item to rebuild the grid panel source file
		final var itemRebuild = new MenuItem(menuForm, SWT.NONE);
		itemRebuild.setText(TITLE_REBUILD);

		itemRebuild.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final FormPanel panel = getSelectedObject();

				try {
					final Project project = panel.getDTO().getNamespace().getProject();

					new FormService(project).rebuildGridPanel(panel);
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

				try {
					final Project project = getSelectedObject().getDTO().getNamespace().getProject();
					final int numberOfNewActions = new FormService(project).syncActions(getSelectedObject());

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

		// Add a menu item to delete the selected grid panel
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

				if (!MessageDialog.openConfirm(shell, TITLE_DELETE, "Do you really want to delete the selected grid panel?"))
					return;

				try {
					final Project project = getSelectedObject().getDTO().getNamespace().getProject();

					new FormService(project).deleteGridPanel(getSelectedObject());

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
