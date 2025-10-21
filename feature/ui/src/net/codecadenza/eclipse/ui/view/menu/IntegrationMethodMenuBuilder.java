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

import java.util.Set;
import java.util.stream.Collectors;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationBean;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod;
import net.codecadenza.eclipse.model.project.IntegrationTechnology;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.testing.IntegrationTestCase;
import net.codecadenza.eclipse.service.integration.IntegrationBeanService;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.eclipse.ui.dialog.integration.EditIntegrationMethodDialog;
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
import org.eclipse.ui.PlatformUI;

/**
 * <p>
 * Helper class to create a context menu for tree view items that represent integration methods
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class IntegrationMethodMenuBuilder extends AbstractMenuBuilder<AbstractIntegrationMethod> {
	/**
	 * Constructor
	 * @param view
	 * @param tree
	 */
	public IntegrationMethodMenuBuilder(ProjectView view, Tree tree) {
		super(view, tree);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.view.menu.AbstractMenuBuilder#createMenu()
	 */
	@Override
	public Menu createMenu() {
		final var menuIntegrationMethod = new Menu(tree);

		// Add a menu item to edit the integration method
		final var itemEdit = new MenuItem(menuIntegrationMethod, SWT.NONE);
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

				final AbstractIntegrationMethod method = getSelectedObject();
				final AbstractIntegrationBean integrationBean = method.getIntegrationBean();
				final Project project = integrationBean.getNamespace().getProject();
				final var integrationBeanService = new IntegrationBeanService(project);

				// Open a dialog to edit the selected method
				final var dlg = new EditIntegrationMethodDialog(shell, method);

				if (dlg.open() == Dialog.OK) {
					// Save changes and rebuild the integration source files
					try {
						final TreeItem integrationItem = getSelectedItem();
						integrationItem.setText(integrationBeanService.getMethodSignature(method));

						EclipseIDEService.saveProjectMetaData(project);

						integrationBeanService.rebuildIntegrationBeanSourceFiles(integrationBean);

						if (integrationBean.getIntegrationTechnology() == IntegrationTechnology.KAFKA)
							integrationBeanService.generateJavaClassesFromAvroIDLFiles();
					}
					catch (final Exception ex) {
						CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
					}
				}

				view.refreshTree();
			}
		});

		// Add a menu item to remove the selected integration method
		final var itemDelete = new MenuItem(menuIntegrationMethod, SWT.NONE);
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

				final AbstractIntegrationMethod method = getSelectedObject();
				final Project project = method.getIntegrationBean().getNamespace().getProject();
				final Set<IntegrationTestCase> testCases = project.searchIntegrationTestCasesByIntegrationMethod(method);

				if (!testCases.isEmpty()) {
					final var message = "The method cannot be deleted, because it is referenced by following integration test cases:\n";

					final String testCaseNames = testCases.stream().map(IntegrationTestCase::getName)
							.collect(Collectors.joining(System.lineSeparator()));

					MessageDialog.openInformation(shell, TITLE_DELETE, message + testCaseNames);
					return;
				}

				if (!MessageDialog.openConfirm(shell, TITLE_DELETE, "Do you really want to delete the selected method?"))
					return;

				try {
					new IntegrationBeanService(project).removeIntegrationMethod(method);

					view.refreshTree();
				}
				catch (final Exception ex) {
					CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
				}
			}
		});

		return menuIntegrationMethod;
	}

}
