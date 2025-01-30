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
package net.codecadenza.runtime.webclient.vaadin.tree;

import com.vaadin.flow.component.AttachEvent;
import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.grid.GridVariant;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.treegrid.TreeGrid;
import com.vaadin.flow.data.provider.hierarchy.TreeData;
import com.vaadin.flow.data.provider.hierarchy.TreeDataProvider;
import java.util.UUID;
import net.codecadenza.runtime.webclient.vaadin.i18n.I18NService;
import net.codecadenza.runtime.webclient.vaadin.util.Navigator;

/**
 * <p>
 * Abstract base class for the application's tree navigator
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractTreeNavigator extends TreeGrid<TreeItem> {
	private static final long serialVersionUID = 5510069502493103761L;
	private static final String FOLDER_TYPE = "folder";
	private static final String VIEW_TYPE = "view";

	private final Navigator navigator = new Navigator(this);
	protected final I18NService i18n;
	protected TreeData<TreeItem> treeData = new TreeData<>();

	/**
	 * Constructor
	 * @param i18n
	 */
	protected AbstractTreeNavigator(I18NService i18n) {
		this.i18n = i18n;
	}

	/**
	 * Add all items to the navigator tree view
	 */
	protected abstract void addTreeItems();

	/*
	 * (non-Javadoc)
	 * @see com.vaadin.flow.component.grid.Grid#onAttach(com.vaadin.flow.component.AttachEvent)
	 */
	@Override
	protected void onAttach(AttachEvent attachEvent) {
		setSizeFull();
		setSelectionMode(SelectionMode.SINGLE);
		addThemeVariants(GridVariant.LUMO_NO_ROW_BORDERS);
		setDataProvider(new TreeDataProvider<>(treeData));
		getStyle().set("border", "0px");

		addItemClickListener(event -> {
			if (event.getItem() == null)
				return;

			final TreeItem navigatorItem = event.getItem();

			if (navigatorItem.getLink() != null && !navigatorItem.getLink().isEmpty())
				getUI().ifPresent(ui -> ui.navigate(navigatorItem.getLink()));
		});

		// Just add one column with the default renderer to the tree view
		TreeGridColumnUtil.addColumn(this);
	}

	/**
	 * Build the tree view
	 */
	public void buildTree() {
		addTreeItems();

		expandRecursively(treeData.getRootItems(), 2);
	}

	/**
	 * Navigate to the view of the given class
	 * @param viewClass
	 */
	public void navigateTo(Class<? extends Component> viewClass) {
		navigator.navigateTo(viewClass);
	}

	/**
	 * Navigate to the view of the given class
	 * @param id the ID that should be added to the route
	 * @param viewClass
	 */
	public void navigateTo(Class<? extends Component> viewClass, String id) {
		navigator.navigateTo(viewClass, id);
	}

	/**
	 * Navigate to the view of the given class
	 * @param id the ID that should be added to the route
	 * @param viewClass
	 */
	public void navigateTo(Class<? extends Component> viewClass, Long id) {
		navigator.navigateTo(viewClass, id);
	}

	/**
	 * Navigate to the view of the given class
	 * @param id the ID that should be added to the route
	 * @param viewClass
	 */
	public void navigateTo(Class<? extends Component> viewClass, Integer id) {
		navigator.navigateTo(viewClass, id);
	}

	/**
	 * Navigate to the view of the given class
	 * @param id the ID that should be added to the route
	 * @param viewClass
	 */
	public void navigateTo(Class<? extends Component> viewClass, UUID id) {
		navigator.navigateTo(viewClass, id);
	}

	/**
	 * Create a new folder tree item and add it to the given parent item
	 * @param parentItem
	 * @param label
	 * @return the new tree item
	 */
	protected TreeItem addFolderItem(TreeItem parentItem, String label) {
		final var folderItem = new TreeItem(FOLDER_TYPE, label);
		folderItem.setIcon(VaadinIcon.FOLDER);

		treeData.addItem(parentItem, folderItem);

		return folderItem;
	}

	/**
	 * Create a new tree item that represents a view and add it to the given parent item
	 * @param parentItem
	 * @param label
	 * @param viewId
	 */
	protected void addViewItem(TreeItem parentItem, String label, String viewId) {
		final var viewItem = new TreeItem(viewId, VIEW_TYPE, label, viewId);
		viewItem.setIcon(VaadinIcon.FILE);

		treeData.addItem(parentItem, viewItem);
	}

}
