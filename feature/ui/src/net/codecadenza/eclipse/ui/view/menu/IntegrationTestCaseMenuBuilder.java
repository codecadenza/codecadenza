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

import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.WorkspaceFile;
import net.codecadenza.eclipse.model.testing.IntegrationTestCase;
import net.codecadenza.eclipse.model.testing.IntegrationTestModule;
import net.codecadenza.eclipse.service.launch.LaunchService;
import net.codecadenza.eclipse.service.testing.integration.IntegrationTestCaseService;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.eclipse.ui.dialog.testing.integration.EditIntegrationTestCaseDialog;
import net.codecadenza.eclipse.ui.view.ProjectView;
import org.eclipse.debug.ui.DebugUITools;
import org.eclipse.debug.ui.IDebugUIConstants;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.MessageDialog;
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
 * Helper class to create a context menu for tree view items that represent an integration test case
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class IntegrationTestCaseMenuBuilder extends AbstractMenuBuilder<IntegrationTestCase> {
	/**
	 * Constructor
	 * @param view
	 * @param tree
	 */
	public IntegrationTestCaseMenuBuilder(ProjectView view, Tree tree) {
		super(view, tree);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.view.menu.AbstractMenuBuilder#createMenu()
	 */
	@Override
	public Menu createMenu() {
		final var menuTestCase = new Menu(tree);

		// Add a menu item to run the test
		final var itemRun = new MenuItem(menuTestCase, SWT.NONE);
		itemRun.setText(TITLE_RUN);
		itemRun.setImage(DebugUITools.getImage(IDebugUIConstants.IMG_ACT_RUN));

		itemRun.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				launchTest(false, TITLE_RUN);
			}
		});

		// Add a menu item to debug the test
		final var itemDebug = new MenuItem(menuTestCase, SWT.NONE);
		itemDebug.setText(TITLE_DEBUG);
		itemDebug.setImage(DebugUITools.getImage(IDebugUIConstants.IMG_ACT_DEBUG));

		itemDebug.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				launchTest(true, TITLE_DEBUG);
			}
		});

		new MenuItem(menuTestCase, SWT.SEPARATOR);

		// Add a menu item to open the test case source file
		final var itemOpen = new MenuItem(menuTestCase, SWT.NONE);
		itemOpen.setText(TITLE_OPEN);

		itemOpen.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				view.openIntegrationTestCaseInEditor(getSelectedItem());
			}
		});

		// Add a menu item to open the test data file
		final var itemTestData = new MenuItem(menuTestCase, SWT.NONE);
		itemTestData.setText(TITLE_OPEN_TEST_DATA);

		itemTestData.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final IntegrationTestCase testCase = getSelectedObject();
				final Project project = testCase.getNamespace().getProject();
				final var path = project.getTestDataFolder() + "/" + testCase.getName() + ".xml";
				final var testDataFile = new WorkspaceFile(project, testCase.getTestModule().getArtifactType(), path, null);

				try {
					EclipseIDEService.openInEditor(testDataFile);
				}
				catch (final Exception ex) {
					CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
				}
			}
		});

		// Add a menu item to edit the selected test case
		final var itemEdit = new MenuItem(menuTestCase, SWT.NONE);
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

				final var dialog = new EditIntegrationTestCaseDialog(shell, getSelectedObject());

				if (dialog.open() == Dialog.OK)
					getSelectedItem().setText(dialog.getTestCase().getName());

				view.refreshTree();
			}
		});

		new MenuItem(menuTestCase, SWT.SEPARATOR);

		// Add a menu item to rebuild the test case source files
		final var itemRebuild = new MenuItem(menuTestCase, SWT.NONE);
		itemRebuild.setText(TITLE_REBUILD);

		itemRebuild.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				try {
					final IntegrationTestModule testModule = (IntegrationTestModule) getSelectedObject().getTestModule();

					new IntegrationTestCaseService(testModule).rebuildTestCaseSourceFiles(getSelectedObject());
				}
				catch (final Exception ex) {
					CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
				}
			}
		});

		// Add a menu item to delete the selected test case
		final var itemDelete = new MenuItem(menuTestCase, SWT.NONE);
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

				if (!MessageDialog.openConfirm(shell, TITLE_DELETE, "Do you really want to delete the selected test case?"))
					return;

				try {
					final IntegrationTestModule testModule = (IntegrationTestModule) getSelectedObject().getTestModule();

					new IntegrationTestCaseService(testModule).deleteTestCase(getSelectedObject());

					view.refreshTree();
				}
				catch (final Exception ex) {
					CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
				}
			}
		});

		return menuTestCase;
	}

	/**
	 * Run or debug the selected test
	 * @param debug
	 * @param title
	 */
	private void launchTest(boolean debug, String title) {
		final IntegrationTestCase testCase = getSelectedObject();
		final Project project = testCase.getNamespace().getProject();

		try {
			if (debug)
				new LaunchService(project).debugTest(testCase.getSourceFile());
			else
				new LaunchService(project).runTest(testCase.getSourceFile());
		}
		catch (final Exception ex) {
			final var message = "Error while launching JUnit test! Message: " + ex.getMessage();

			MessageDialog.openWarning(shell, title, message);
		}
	}

}
