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

import net.codecadenza.eclipse.model.integration.AbstractIntegrationBean;
import net.codecadenza.eclipse.model.project.IntegrationTechnology;
import net.codecadenza.eclipse.service.integration.IntegrationBeanService;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.eclipse.ui.dialog.integration.EditIntegrationBeanDialog;
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
 * Helper class to create a context menu for tree view items that represent integration beans
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class IntegrationBeanMenuBuilder extends AbstractMenuBuilder<AbstractIntegrationBean> {
	/**
	 * Constructor
	 * @param view
	 * @param tree
	 */
	public IntegrationBeanMenuBuilder(ProjectView view, Tree tree) {
		super(view, tree);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.view.menu.AbstractMenuBuilder#createMenu()
	 */
	@Override
	public Menu createMenu() {
		final var menuIntegrationBean = new Menu(tree);

		// Add a menu item to open the integration bean source file
		final var itemOpen = new MenuItem(menuIntegrationBean, SWT.NONE);
		itemOpen.setText(TITLE_OPEN);

		itemOpen.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				view.openIntegrationBeanInEditor(getSelectedItem());
			}
		});

		// Add a menu item to edit the selected integration bean
		final var itemNewMethod = new MenuItem(menuIntegrationBean, SWT.NONE);
		itemNewMethod.setText(TITLE_EDIT);

		itemNewMethod.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (EclipseIDEService.openDiagramExists(view))
					return;

				final AbstractIntegrationBean integrationBean = getSelectedObject();
				final TreeItem selectedItem = getSelectedItem();

				new EditIntegrationBeanDialog(shell, integrationBean).open();

				view.addIntegrationBeanChildItems(selectedItem, integrationBean);
				selectedItem.setExpanded(true);

				// Determine the integration bean as loaded
				ProjectTreeViewHelper.setJavaTypeLoaded(integrationBean.hashCode());

				view.refreshTree();
			}
		});

		new MenuItem(menuIntegrationBean, SWT.SEPARATOR);

		// Add a menu item to rebuild the integration bean source files
		final var itemRebuild = new MenuItem(menuIntegrationBean, SWT.NONE);
		itemRebuild.setText(TITLE_REBUILD);

		itemRebuild.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				try {
					final var integrationBeanService = new IntegrationBeanService(getSelectedObject().getNamespace().getProject());
					integrationBeanService.rebuildIntegrationBeanSourceFiles(getSelectedObject());

					if (getSelectedObject().getIntegrationTechnology() == IntegrationTechnology.KAFKA)
						integrationBeanService.generateJavaClassesFromAvroIDLFiles();
				}
				catch (final Exception ex) {
					CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
				}
			}
		});

		// Add a menu item to refresh the integration bean methods
		final var itemRefresh = new MenuItem(menuIntegrationBean, SWT.NONE);
		itemRefresh.setText(TITLE_REFRESH);

		itemRefresh.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final TreeItem selItem = getSelectedItem();
				final AbstractIntegrationBean integrationBean = getSelectedObject();

				view.addIntegrationBeanChildItems(selItem, integrationBean);

				// Determine the integration bean as loaded
				ProjectTreeViewHelper.setJavaTypeLoaded(integrationBean.hashCode());
			}
		});

		// Add a menu item to delete the selected integration bean
		final var itemDelete = new MenuItem(menuIntegrationBean, SWT.NONE);
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

				if (!MessageDialog.openConfirm(shell, TITLE_DELETE, "Do you really want to delete the selected integration bean?"))
					return;

				try {
					final var integrationBeanService = new IntegrationBeanService(getSelectedObject().getNamespace().getProject());
					integrationBeanService.removeIntegrationBean(getSelectedObject());

					view.refreshTree();
				}
				catch (final Exception ex) {
					CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
				}
			}
		});

		return menuIntegrationBean;
	}

}
