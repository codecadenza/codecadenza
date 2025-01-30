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

import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.service.boundary.BoundaryService;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.eclipse.ui.dialog.boundary.EditBoundaryBeanDialog;
import net.codecadenza.eclipse.ui.view.ProjectView;
import net.codecadenza.eclipse.ui.view.util.ProjectTreeViewHelper;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;

/**
 * <p>
 * Helper class to create a context menu for tree view items that represent boundary beans
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class BoundaryBeanMenuBuilder extends AbstractMenuBuilder<BoundaryBean> {
	/**
	 * Constructor
	 * @param view
	 * @param tree
	 */
	public BoundaryBeanMenuBuilder(ProjectView view, Tree tree) {
		super(view, tree);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.view.menu.AbstractMenuBuilder#createMenu()
	 */
	@Override
	public Menu createMenu() {
		final var menuBoundaryBean = new Menu(tree);

		// Add a menu item to open a boundary bean source file
		final var itemOpen = new MenuItem(menuBoundaryBean, SWT.NONE);
		itemOpen.setText(TITLE_OPEN);

		itemOpen.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				view.openBoundaryInEditor(getSelectedItem());
			}
		});

		// Add a menu item to edit the selected boundary bean
		final var itemEdit = new MenuItem(menuBoundaryBean, SWT.NONE);
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

				final BoundaryBean boundaryBean = getSelectedObject();
				final TreeItem selectedItem = getSelectedItem();

				new EditBoundaryBeanDialog(shell, boundaryBean).open();

				view.addBoundaryChildItems(selectedItem, boundaryBean);
				selectedItem.setExpanded(true);

				// Determine the boundary bean as loaded
				ProjectTreeViewHelper.setJavaTypeLoaded(boundaryBean.hashCode());

				view.refreshTree();
			}
		});

		new MenuItem(menuBoundaryBean, SWT.SEPARATOR);

		// Add a menu item to rebuild the boundary source files
		final var itemRebuild = new MenuItem(menuBoundaryBean, SWT.NONE);
		itemRebuild.setText(TITLE_REBUILD);

		itemRebuild.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				try {
					final Project project = getSelectedObject().getNamespace().getProject();

					new BoundaryService(project).rebuildBoundarySourceFiles(getSelectedObject());
				}
				catch (final Exception ex) {
					CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
				}
			}
		});

		// Add a menu item to refresh the boundary bean methods
		final var itemRefresh = new MenuItem(menuBoundaryBean, SWT.NONE);
		itemRefresh.setText(TITLE_REFRESH);

		itemRefresh.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final TreeItem selItem = getSelectedItem();
				final BoundaryBean boundaryBean = getSelectedObject();

				view.addBoundaryChildItems(selItem, boundaryBean);

				// Determine the boundary bean as loaded
				ProjectTreeViewHelper.setJavaTypeLoaded(boundaryBean.hashCode());
			}
		});

		// Add a menu item to delete the selected boundary
		final var itemDelete = new MenuItem(menuBoundaryBean, SWT.NONE);
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

				if (!MessageDialog.openConfirm(shell, TITLE_DELETE, "Do you really want to delete the selected boundary bean?"))
					return;

				try {
					final Project project = getSelectedObject().getNamespace().getProject();

					new BoundaryService(project).removeBoundary(getSelectedObject());

					view.refreshTree();
				}
				catch (final Exception ex) {
					CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
				}
			}
		});

		return menuBoundaryBean;
	}

}
