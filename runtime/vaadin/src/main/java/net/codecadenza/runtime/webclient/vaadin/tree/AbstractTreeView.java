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

import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.CMD_REFRESH;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.MSG_COUNT_ERROR;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.MSG_COUNT_RESULT;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.MSG_COUNT_TITLE;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.MSG_DATA_FETCH;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.MSG_DATA_FETCH_NO_COUNT;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.MSG_DATA_FETCH_NO_RESULTS;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.MSG_DATA_FETCH_WITH_COUNT;
import static net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService.MSG_UNEXPECTED_ERROR_TITLE;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.grid.Grid.SelectionMode;
import com.vaadin.flow.component.grid.GridVariant;
import com.vaadin.flow.component.grid.contextmenu.GridContextMenu;
import com.vaadin.flow.component.html.NativeLabel;
import com.vaadin.flow.component.icon.Icon;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.treegrid.TreeGrid;
import com.vaadin.flow.data.provider.hierarchy.TreeData;
import com.vaadin.flow.data.provider.hierarchy.TreeDataProvider;
import jakarta.annotation.PostConstruct;
import java.text.DecimalFormat;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import net.codecadenza.runtime.search.dto.SearchDTO;
import net.codecadenza.runtime.webclient.vaadin.dialog.ErrorMessageDialog;
import net.codecadenza.runtime.webclient.vaadin.dialog.InfoMessageDialog;
import net.codecadenza.runtime.webclient.vaadin.i18n.I18NService;
import net.codecadenza.runtime.webclient.vaadin.i18n.InternalI18NService;
import net.codecadenza.runtime.webclient.vaadin.search.SearchInputDialog;
import net.codecadenza.runtime.webclient.vaadin.util.Navigator;
import net.codecadenza.runtime.webclient.vaadin.util.PreferencesStore;
import org.slf4j.Logger;

/**
 * <p>
 * Abstract base class for all generated tree views
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractTreeView extends VerticalLayout {
	private static final long serialVersionUID = 7871410458759901066L;
	private static final long DEFAULT_COUNT_RESULT = 0;
	private static final String EMPTY_TITLE = "";

	protected final PreferencesStore preferences;
	protected final I18NService i18n;
	protected final Navigator navigator = new Navigator(this);
	protected final TreeGrid<TreeItem> tree = new TreeGrid<>();
	protected final NativeLabel lblFooter = new NativeLabel();
	protected final GridContextMenu<TreeItem> contextMenu = tree.addContextMenu();
	protected TreeData<TreeItem> treeData = new TreeData<>();
	protected SearchDTO searchInput;
	protected DecimalFormat decimalFormat;
	protected transient DateTimeFormatter dateTimeFormat;
	protected transient DateTimeFormatter dateFormat;
	protected InternalI18NService internalI18n;

	/**
	 * Constructor
	 * @param i18n
	 * @param preferences
	 */
	protected AbstractTreeView(I18NService i18n, PreferencesStore preferences) {
		this.i18n = i18n;
		this.preferences = preferences;
	}

	/**
	 * The way items should be searched and added to the tree must be defined by the implementation class!
	 */
	protected abstract void addRootTreeItems();

	/**
	 * Every non-abstract subclass must provide a logger instance!
	 * @return the logger
	 */
	protected abstract Logger getTreeViewLogger();

	/**
	 * Initialize the tree view
	 */
	@PostConstruct
	public void initComponent() {
		setSizeFull();

		decimalFormat = new DecimalFormat(preferences.getNumberFormat());
		dateTimeFormat = DateTimeFormatter.ofPattern(preferences.getDateTimeFormat()).withZone(ZoneId.systemDefault());
		dateFormat = DateTimeFormatter.ofPattern(preferences.getDateFormat()).withZone(ZoneId.systemDefault());
		internalI18n = new InternalI18NService(i18n.getLocale());

		getTreeViewLogger().debug("Initialize tree view");

		add(initializeHeader());

		if (getQuickSearchPanel() != null)
			add(getQuickSearchPanel());

		tree.setSizeFull();
		tree.setSelectionMode(SelectionMode.SINGLE);
		tree.addThemeVariants(GridVariant.LUMO_NO_ROW_BORDERS);
		tree.setDataProvider(new TreeDataProvider<>(treeData));

		// Just add one column with the default renderer to the tree
		TreeGridColumnUtil.addColumn(tree);

		tree.addExpandListener(event -> {
			if (event.getItems().stream().findFirst().isEmpty())
				return;

			final var selectedItem = event.getItems().stream().findFirst().get();

			tree.select(selectedItem);

			onNodeExpand(selectedItem);
		});

		tree.addSelectionListener(_ -> {
			contextMenu.removeAll();

			final var selectedItem = getSelectedItem();

			if (selectedItem == null)
				return;

			addContextMenuItems(selectedItem);
		});

		lblFooter.setText(internalI18n.getTranslation(MSG_DATA_FETCH_NO_RESULTS));

		add(tree, lblFooter, contextMenu);

		getTreeViewLogger().debug("Tree view initialization finished");
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
	 * Open the search input dialog
	 */
	protected void openSearchDialog() {
		final var dlg = new SearchInputDialog(searchInput, internalI18n.getLocale());

		dlg.addOpenedChangeListener(event -> {
			if (event.isOpened() || dlg.getMode() == SearchInputDialog.OperationMode.NONE)
				return;

			if (dlg.getMode() == SearchInputDialog.OperationMode.COUNT) {
				getTreeViewLogger().debug("Perform count operation");

				try {
					final long countResult = performCountOperation();

					final String message = internalI18n.getTranslation(MSG_COUNT_RESULT, countResult);
					final String title = internalI18n.getTranslation(MSG_COUNT_TITLE);

					new InfoMessageDialog(title, message, internalI18n.getLocale()).open();
				}
				catch (final Exception ex) {
					getTreeViewLogger().error("Error while performing count operation!", ex);

					final String title = internalI18n.getTranslation(MSG_UNEXPECTED_ERROR_TITLE);
					final String message = internalI18n.getTranslation(MSG_COUNT_ERROR);

					new ErrorMessageDialog(title, message, ex, internalI18n.getLocale()).open();
				}
			}
			else
				addRootTreeItems();
		});

		dlg.open();
	}

	/**
	 * @param numberOfRecords
	 * @param countResult
	 * @param timeDif
	 * @return the generated footer message
	 */
	protected String buildFooterMessage(Long numberOfRecords, Long countResult, Long timeDif) {
		final var params = new ArrayList<>();
		params.add(numberOfRecords);

		if (timeDif == null)
			return internalI18n.getTranslation(MSG_DATA_FETCH, params.toArray());

		if (countResult != null) {
			params.add(countResult);
			params.add(String.format("%.2f", (double) timeDif / 1000));

			return internalI18n.getTranslation(MSG_DATA_FETCH_WITH_COUNT, params.toArray());
		}

		params.add(String.format("%.2f", (double) timeDif / 1000));

		return internalI18n.getTranslation(MSG_DATA_FETCH_NO_COUNT, params.toArray());
	}

	/**
	 * @param hlButtons
	 */
	protected void addHeaderButtons(HorizontalLayout hlButtons) {
		final var cmdRefresh = new Button(internalI18n.getTranslation(CMD_REFRESH));
		cmdRefresh.setIcon(new Icon(VaadinIcon.REFRESH));
		cmdRefresh.setId("cmdRefresh");
		cmdRefresh.addClickListener(_ -> addRootTreeItems());

		hlButtons.add(cmdRefresh);
	}

	/**
	 * Initialize the header
	 * @return the layout that contains the header
	 */
	protected HorizontalLayout initializeHeader() {
		final var hlButtons = new HorizontalLayout();

		addHeaderButtons(hlButtons);

		return hlButtons;
	}

	/**
	 * Callback method that notifies the receiver that a count operation should be triggered
	 * @return the number of records found
	 */
	protected long performCountOperation() {
		return DEFAULT_COUNT_RESULT;
	}

	/**
	 * Callback method that notifies the receiver that a tree item has been expanded
	 * @param expandedItem
	 */
	@SuppressWarnings("unused")
	protected void onNodeExpand(TreeItem expandedItem) {

	}

	/**
	 * Callback method that notifies the receiver that a tree item has been dropped
	 * @param dragItem
	 * @param dropItem
	 */
	@SuppressWarnings("unused")
	protected void onDropItem(TreeItem dragItem, TreeItem dropItem) {

	}

	/**
	 * The implementation class should overwrite this method in order to add necessary context menu items
	 * @param selectedItem represents the currently selected tree item
	 */
	@SuppressWarnings("unused")
	protected void addContextMenuItems(TreeItem selectedItem) {

	}

	/**
	 * Refresh the tree view
	 */
	protected void refreshTree() {
		tree.getDataProvider().refreshAll();
	}

	/**
	 * @return the tree view title
	 */
	protected String getTitle() {
		return EMPTY_TITLE;
	}

	/**
	 * @return a component that in turn contains all components for performing quick-search operations
	 */
	protected Component getQuickSearchPanel() {
		return null;
	}

	/**
	 * @return the selected item
	 */
	protected TreeItem getSelectedItem() {
		if (tree.getSelectedItems() == null)
			return null;

		return tree.getSelectedItems().stream().findFirst().orElse(null);
	}

	/**
	 * @param treeItem
	 * @return the parent of the given item. It will return null if the item has no parent!
	 */
	protected TreeItem getParentItem(TreeItem treeItem) {
		return tree.getTreeData().getParent(treeItem);
	}

	/**
	 * Add a tree item to the given parent item
	 * @param parentItem
	 * @param treeItem
	 * @param addDummyItem
	 */
	protected void addItem(TreeItem parentItem, TreeItem treeItem, boolean addDummyItem) {
		treeData.addItem(parentItem, treeItem);

		if (addDummyItem)
			treeData.addItem(treeItem, new TreeItem("", ""));
	}

	/**
	 * Add a tree item to the given parent item
	 * @param parentItem
	 * @param treeItem
	 */
	protected void addItem(TreeItem parentItem, TreeItem treeItem) {
		addItem(parentItem, treeItem, false);
	}

	/**
	 * Remove the given item from the tree view
	 * @param treeItem
	 */
	protected void removeItem(TreeItem treeItem) {
		treeData.removeItem(treeItem);

		refreshTree();
	}

	/**
	 * Remove all children of the given item from the tree view
	 * @param parentItem
	 */
	protected void removeItemsOfParent(TreeItem parentItem) {
		// Copy all children of the given item into a new list
		final List<TreeItem> children = new ArrayList<>(treeData.getChildren(parentItem));

		// Remove all children from the tree data structure
		children.forEach(treeData::removeItem);
	}

	/**
	 * Remove the currently selected item from the tree view
	 */
	protected void removeSelectedItem() {
		final TreeItem item = getSelectedItem();

		if (item == null)
			return;

		removeItem(item);
	}

	/**
	 * Remove all tree items
	 */
	protected void removeAllItems() {
		treeData.clear();
	}

}
