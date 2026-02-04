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

import net.codecadenza.eclipse.ui.view.ProjectView;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;

/**
 * <p>
 * Abstract base class for all project menu builders
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the object that is connected to a selected tree view item
 */
public abstract class AbstractMenuBuilder<T> {
	public static final String TITLE_OPEN = "Open";
	public static final String TITLE_NEW = "New";
	public static final String TITLE_SYNC = "Synchronize";
	public static final String TITLE_EDIT = "Edit";
	public static final String TITLE_ADD = "Add";
	public static final String TITLE_REBUILD = "Rebuild";
	public static final String TITLE_REFRESH = "Refresh";
	public static final String TITLE_DELETE = "Delete";
	public static final String TITLE_RENAME = "Rename";
	public static final String TITLE_PROP = "Properties";
	public static final String TITLE_QUERY = "Query";
	public static final String TITLE_REBUILD_ALL = "Rebuild all files";
	public static final String TITLE_PREVIEW = "Preview";
	public static final String TITLE_SYNC_ACTIONS = "Synchronize actions";
	public static final String TITLE_DEPLOY = "Deploy";
	public static final String TITLE_CREATE_LOG_ON = "Create log-on DTO";
	public static final String TITLE_EDIT_PROJECT = "Edit";
	public static final String TITLE_EDIT_DATA_SOURCE = "Data source";
	public static final String TITLE_EDIT_ROLES = "Roles";
	public static final String TITLE_REBUILD_NAV = "Rebuild navigator";
	public static final String TITLE_EDIT_PU = "Persistence Unit";
	public static final String TITLE_REBUILD_LOG = "Rebuild logging";
	public static final String TITLE_REBUILD_SEC = "Rebuild security";
	public static final String TITLE_REBUILD_SAVED_QUERY = "Rebuild saved query service";
	public static final String TITLE_CREATE = "Create";
	public static final String TITLE_RECREATE = "Recreate";
	public static final String TITLE_DROP = "Drop";
	public static final String TITLE_REV_ENG = "Reverse engineering";
	public static final String TITLE_REBUILD_SCHEMA = "Rebuild schema file";
	public static final String TITLE_NEW_EXCHANGE_METHOD = "New data exchange method";
	public static final String TITLE_CREATE_FORMS = "Create default forms";
	public static final String TITLE_CREATE_VIEW = "Create view form";
	public static final String TITLE_CREATE_GRID = "Create grid panel";
	public static final String TITLE_CREATE_FORM = "Create update form";
	public static final String TITLE_CREATE_TREE = "Create tree view";
	public static final String TITLE_NEW_INTEGRATION_BEAN = "New integration bean";
	public static final String TITLE_NEW_TEST_CASE = "New test case";
	public static final String TITLE_NEW_TEST_SUITE = "New test suite";
	public static final String TITLE_OPEN_TEST_DATA = "Open test data";
	public static final String TITLE_RUN = "Run";
	public static final String TITLE_DEBUG = "Debug";
	public static final String TITLE_BUILD = "Build";

	protected final Shell shell;
	protected final Tree tree;
	protected final ProjectView view;

	/**
	 * Constructor
	 * @param view
	 * @param tree
	 */
	protected AbstractMenuBuilder(ProjectView view, Tree tree) {
		this.view = view;
		this.tree = tree;
		this.shell = view.getViewSite().getShell();
	}

	/**
	 * @return the selected tree item
	 */
	public TreeItem getSelectedItem() {
		TreeItem selItem = null;
		final TreeItem[] selItems = tree.getSelection();

		for (final TreeItem item : selItems)
			selItem = item;

		return selItem;
	}

	/**
	 * @return the object that is connected to the selected tree item
	 */
	@SuppressWarnings("unchecked")
	public T getSelectedObject() {
		final TreeItem selItem = getSelectedItem();

		if (selItem == null || selItem.getData() == null)
			return null;

		return (T) selItem.getData();
	}

	/**
	 * @return the created menu
	 */
	public abstract Menu createMenu();

}
