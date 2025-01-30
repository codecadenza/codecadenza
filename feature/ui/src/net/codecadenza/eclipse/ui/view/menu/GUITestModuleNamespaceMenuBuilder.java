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

import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.testing.AbstractTestModule;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.ui.dialog.testing.EditTestSuiteDialog;
import net.codecadenza.eclipse.ui.dialog.testing.gui.EditGUITestCaseDialog;
import net.codecadenza.eclipse.ui.view.ProjectView;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Tree;

/**
 * <p>
 * Helper class to create a context menu for tree view items that represent GUI test modules
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class GUITestModuleNamespaceMenuBuilder extends AbstractMenuBuilder<Namespace> {
	/**
	 * Constructor
	 * @param view
	 * @param tree
	 */
	public GUITestModuleNamespaceMenuBuilder(ProjectView view, Tree tree) {
		super(view, tree);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.view.menu.AbstractMenuBuilder#createMenu()
	 */
	@Override
	public Menu createMenu() {
		final var menuGUITestModule = new Menu(tree);

		// Add a menu item to create a new test case
		final var itemAddTestCase = new MenuItem(menuGUITestModule, SWT.NONE);
		itemAddTestCase.setText(TITLE_NEW_TEST_CASE);

		itemAddTestCase.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (EclipseIDEService.openDiagramExists(view))
					return;

				final Namespace namespace = getSelectedObject();
				final Project project = namespace.getProject();

				for (final AbstractTestModule module : project.getTestModules())
					if (module.getNamespace().equals(namespace)) {
						new EditGUITestCaseDialog(shell, module).open();
						break;
					}

				view.refreshTree();
			}
		});

		// Add a menu item to create a new test suite
		final var itemAddTestSuite = new MenuItem(menuGUITestModule, SWT.NONE);
		itemAddTestSuite.setText(TITLE_NEW_TEST_SUITE);

		itemAddTestSuite.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (EclipseIDEService.openDiagramExists(view))
					return;

				final Namespace namespace = getSelectedObject();
				final Project project = namespace.getProject();

				for (final AbstractTestModule module : project.getTestModules())
					if (module.getNamespace().equals(namespace)) {
						new EditTestSuiteDialog(shell, module).open();
						break;
					}

				view.refreshTree();
			}
		});

		return menuGUITestModule;
	}

}
