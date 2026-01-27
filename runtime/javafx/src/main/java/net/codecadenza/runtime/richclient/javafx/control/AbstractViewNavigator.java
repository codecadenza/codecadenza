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
package net.codecadenza.runtime.richclient.javafx.control;

import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.ABSTRACT_VIEW_NAVIGATOR_MSG_DELETE_QUERY;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.ABSTRACT_VIEW_NAVIGATOR_MSG_DELETE_QUERY_TITLE;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.ABSTRACT_VIEW_NAVIGATOR_ROOT_ITEM_LABEL;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.ABSTRACT_VIEW_NAVIGATOR_SAVED_QUERIES_LABEL;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.ACTION_DELETE_TITLE;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.getTranslation;
import static net.codecadenza.runtime.richclient.javafx.image.ImageLoader.IMG_FOLDER;
import static net.codecadenza.runtime.richclient.javafx.image.ImageLoader.IMG_VIEW;
import static net.codecadenza.runtime.richclient.javafx.image.ImageLoader.getImage;

import java.lang.invoke.MethodHandles;
import java.lang.reflect.Constructor;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.MenuItem;
import javafx.scene.control.TabPane;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import javafx.scene.image.ImageView;
import net.codecadenza.runtime.richclient.javafx.dialog.DialogButtonType;
import net.codecadenza.runtime.richclient.javafx.dialog.DialogUtil;
import net.codecadenza.runtime.richclient.search.event.SearchDTOChangeController;
import net.codecadenza.runtime.richclient.search.event.SearchDTOChangeListener;
import net.codecadenza.runtime.richclient.search.util.SearchManager;
import net.codecadenza.runtime.search.dto.SearchDTO;
import net.codecadenza.runtime.search.dto.SearchListDTO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Abstract base class for application tree navigators
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractViewNavigator extends TreeView<View> implements SearchDTOChangeListener {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	private TreeItem<View> rootItem;
	private TreeItem<View> savedQueriesItem;
	private final TabPane viewParent;

	/**
	 * Constructor
	 * @param viewParent
	 */
	protected AbstractViewNavigator(TabPane viewParent) {
		this.viewParent = viewParent;

		SearchDTOChangeController.addSearchDTOChangeListener(this);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.search.event.SearchDTOChangeListener#onNewSavedQuery(net.codecadenza.runtime.
	 * .search.dto.SearchListDTO)
	 */
	@Override
	public void onNewSavedQuery(SearchListDTO savedSearch) {
		addSavedSearch(savedSearch);
	}

	/**
	 * Add the items to the tree view
	 * @param rootItem
	 */
	public abstract void addTreeItems(TreeItem<View> rootItem);

	/**
	 * Create the root tree item
	 */
	protected void addRootItem() {
		rootItem = new TreeItem<>(new ViewGroup(getTranslation(ABSTRACT_VIEW_NAVIGATOR_ROOT_ITEM_LABEL)));
		rootItem.setGraphic(new ImageView(getImage(IMG_FOLDER)));

		setRoot(rootItem);

		rootItem.setExpanded(true);
	}

	/**
	 * Add a saved search to the tree view
	 * @param savedSearch
	 */
	@SuppressWarnings("unchecked")
	private void addSavedSearch(SearchListDTO savedSearch) {
		try {
			final var cl = (Class<View>) Class.forName(savedSearch.getViewName());
			final Constructor<View> cons = cl.getConstructor(String.class, SearchDTO.class, Integer.class);
			final View view = cons.newInstance(savedSearch.getName(), SearchManager.getSavedSearch(savedSearch.getId()),
					savedSearch.getId());

			final var treeItem = new TreeItem<>(view);
			treeItem.setGraphic(new ImageView(getImage(IMG_VIEW)));

			savedQueriesItem.getChildren().add(treeItem);
		}
		catch (final Exception e) {
			logger.error("Could not add saved search to navigator tree view!", e);
		}
	}

	/**
	 * Add all saved queries to the tree view
	 */
	private void addSavedQueries() {
		savedQueriesItem = new TreeItem<>(new ViewGroup(getTranslation(ABSTRACT_VIEW_NAVIGATOR_SAVED_QUERIES_LABEL)));
		savedQueriesItem.setGraphic(new ImageView(getImage(IMG_FOLDER)));

		rootItem.getChildren().add(savedQueriesItem);

		SearchManager.getAllSavedSearchObjects().forEach(this::addSavedSearch);

		savedQueriesItem.setExpanded(true);
	}

	/**
	 * Delete the selected saved query
	 */
	private void deleteSavedQuery() {
		final String message = getTranslation(ABSTRACT_VIEW_NAVIGATOR_MSG_DELETE_QUERY);
		final String title = getTranslation(ABSTRACT_VIEW_NAVIGATOR_MSG_DELETE_QUERY_TITLE);

		if (DialogButtonType.YES != DialogUtil.openConfirmationDialog(null, title, message))
			return;

		final TreeItem<View> selectedItem = getSelectionModel().getSelectedItem();
		final Integer id = selectedItem.getValue().getSavedQueryId();

		SearchManager.deleteSavedSearchObject(id);

		savedQueriesItem.getChildren().remove(selectedItem);
	}

	/**
	 * Initialize the navigator
	 */
	public void initialize() {
		addRootItem();

		addTreeItems(rootItem);

		addSavedQueries();

		final var mnuTree = new ContextMenu();

		final var mniDelete = new MenuItem(getTranslation(ACTION_DELETE_TITLE));
		mniDelete.setOnAction(_ -> deleteSavedQuery());

		mnuTree.getItems().add(mniDelete);

		// Add a context menu for tree view items that represent a saved query
		setOnContextMenuRequested(event -> {
			mnuTree.hide();
			setContextMenu(null);

			if (getSelectionModel().getSelectedItem() == null)
				return;

			if (getSelectionModel().getSelectedItem().getValue().getTab() == null)
				return;

			if (getSelectionModel().getSelectedItem().getValue().getSavedQueryId() == null)
				return;

			setContextMenu(mnuTree);
			mnuTree.show(AbstractViewNavigator.this, event.getScreenX(), event.getScreenY());
		});

		// Open the selected view
		setOnMouseClicked(e -> {
			if (e.getClickCount() != 2)
				return;

			if (getSelectionModel().getSelectedItem() == null)
				return;

			final View view = getSelectionModel().getSelectedItem().getValue();

			if (view == null || view.getTab() == null)
				return;

			if (!viewParent.getTabs().contains(view.getTab())) {
				viewParent.getTabs().add(view.getTab());

				view.getTab().setText(getSelectionModel().getSelectedItem().getValue().toString());
				view.initialize();
			}

			viewParent.getSelectionModel().select(view.getTab());
		});
	}

}
