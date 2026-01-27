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
package net.codecadenza.runtime.richclient.swing.widget;

import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.ABSTRACT_NAVIGATION_PANEL_MNI_DELETE;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.ABSTRACT_NAVIGATION_PANEL_MSG_DELETE_QUERY;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.ABSTRACT_NAVIGATION_PANEL_MSG_DELETE_QUERY_TITLE;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.ABSTRACT_NAVIGATION_PANEL_ROOT_ITEM_LABEL;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.ABSTRACT_NAVIGATION_PANEL_SAVED_QUERIES_LABEL;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.getTranslation;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.beans.PropertyVetoException;
import java.lang.invoke.MethodHandles;
import java.lang.reflect.Constructor;
import javax.swing.JDesktopPane;
import javax.swing.JInternalFrame;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreePath;
import net.codecadenza.runtime.richclient.search.event.SearchDTOChangeController;
import net.codecadenza.runtime.richclient.search.event.SearchDTOChangeListener;
import net.codecadenza.runtime.richclient.search.util.SearchManager;
import net.codecadenza.runtime.richclient.swing.image.ImageLoader;
import net.codecadenza.runtime.richclient.swing.search.AbstractSearchResultView;
import net.codecadenza.runtime.search.dto.SearchListDTO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Abstract base class for the application's tree navigator panel
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractNavigationPanel extends JPanel implements SearchDTOChangeListener {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static final long serialVersionUID = -2834082051956852535L;

	private JDataTree tree;
	private JDesktopPane desktop;
	private DataTreeNode savedQueriesNode;
	private DataTreeNode rootNode;

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.search.event.SearchDTOChangeListener#onNewSavedQuery(net.codecadenza.runtime.
	 * search.dto.SearchListDTO)
	 */
	@Override
	public void onNewSavedQuery(SearchListDTO dto) {
		new DataTreeNode(savedQueriesNode, dto.getName(), ImageLoader.getImage(ImageLoader.SAVED_SEARCH), dto);
	}

	/**
	 * Initialize the tree structure
	 * @param rootNode
	 */
	public abstract void initializeNodes(DataTreeNode rootNode);

	/**
	 * @param view
	 */
	public void openStandardView(JInternalFrame view) {
		if (view.isVisible()) {
			try {
				if (view.isIcon())
					view.setIcon(false);

				view.setSelected(true);
			}
			catch (final PropertyVetoException ex) {
				logger.warn("Could not change property of view '{}'!", view.getName(), ex);
			}

			view.toFront();

			return;
		}

		view.setLocation(10, 10);
		view.setSize(660, 500);

		desktop.add(view);

		view.setVisible(true);
	}

	/**
	 * Open a saved query
	 * @param searchObj
	 */
	@SuppressWarnings("unchecked")
	private void openSavedQuery(SearchListDTO searchObj) {
		try {
			final var cl = (Class<AbstractSearchResultView<?>>) Class.forName(searchObj.getViewName());
			final Constructor<AbstractSearchResultView<?>> cons = cl.getConstructor(String.class, Integer.class);
			final AbstractSearchResultView<?> view = cons.newInstance(searchObj.getName(), searchObj.getId());

			openStandardView(view);
			view.performFetch();
		}
		catch (final Exception e) {
			logger.error("Error while opening view '{}' with selected saved query!", searchObj.getViewName(), e);
		}
	}

	/**
	 * Constructor
	 * @param desktop
	 */
	protected AbstractNavigationPanel(final JDesktopPane desktop) {
		this.desktop = desktop;

		// Listen for new saved queries
		SearchDTOChangeController.addSearchDTOChangeListener(this);

		// Create the root node of the tree
		rootNode = new DataTreeNode(null, getTranslation(ABSTRACT_NAVIGATION_PANEL_ROOT_ITEM_LABEL),
				ImageLoader.getImage(ImageLoader.FOLDER));

		// Delegate the initialization of the tree
		initializeNodes(rootNode);

		savedQueriesNode = new DataTreeNode(rootNode, getTranslation(ABSTRACT_NAVIGATION_PANEL_SAVED_QUERIES_LABEL),
				ImageLoader.getImage(ImageLoader.FOLDER));

		// Add user defined queries
		SearchManager.getAllSavedSearchObjects()
				.forEach(dto -> new DataTreeNode(savedQueriesNode, dto.getName(), ImageLoader.getImage(ImageLoader.SAVED_SEARCH), dto));

		tree = new JDataTree(rootNode);

		tree.addMouseListener(new MouseAdapter() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.MouseAdapter#mouseClicked(java.awt.event.MouseEvent)
			 */
			@Override
			public void mouseClicked(MouseEvent e) {
				if (e.getClickCount() == 2) {
					final TreePath path = tree.getSelectionPath();

					if (path == null)
						return;

					final var node = (DataTreeNode) path.getLastPathComponent();

					if (node == null)
						return;

					if (node.getUserObject() == null)
						return;

					if (node.getUserObject() instanceof final SearchListDTO searchDTO)
						openSavedQuery(searchDTO);
					else
						openStandardView((JInternalFrame) node.getUserObject());
				}
			}
		});

		final var scrollPane = new JScrollPane();
		scrollPane.setViewportView(tree);

		final var popupMenu = new JPopupMenu();

		addPopup(tree, popupMenu);

		final var mniDelete = new JMenuItem(getTranslation(ABSTRACT_NAVIGATION_PANEL_MNI_DELETE));

		mniDelete.addActionListener(_ -> {
			final var listDTO = (SearchListDTO) tree.getSelectedUserObject();
			final String dialogTitle = getTranslation(ABSTRACT_NAVIGATION_PANEL_MSG_DELETE_QUERY_TITLE);

			if (listDTO == null)
				return;

			final int resp = JOptionPane.showConfirmDialog(AbstractNavigationPanel.this,
					getTranslation(ABSTRACT_NAVIGATION_PANEL_MSG_DELETE_QUERY), dialogTitle, JOptionPane.YES_NO_OPTION);

			if (resp != JOptionPane.YES_OPTION)
				return;

			SearchManager.deleteSavedSearchObject(listDTO.getId());

			// Remove the respective node from the tree
			final var model = (DefaultTreeModel) getTree().getModel();
			model.removeNodeFromParent(tree.getSelectedTreeNode());
		});

		setLayout(new BorderLayout(0, 0));
		popupMenu.add(mniDelete);
		add(scrollPane);

		// Expand all nodes!
		tree.expandAll();
	}

	/**
	 * @return the tree component
	 */
	public JDataTree getTree() {
		return tree;
	}

	/**
	 * @param component
	 * @param popup
	 */
	private void addPopup(Component component, final JPopupMenu popup) {
		component.addMouseListener(new MouseAdapter() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.MouseAdapter#mousePressed(java.awt.event.MouseEvent)
			 */
			@Override
			public void mousePressed(MouseEvent e) {
				if (e.isPopupTrigger())
					showMenu(e);
			}

			/*
			 * (non-Javadoc)
			 * @see java.awt.event.MouseAdapter#mouseReleased(java.awt.event.MouseEvent)
			 */
			@Override
			public void mouseReleased(MouseEvent e) {
				if (e.isPopupTrigger())
					showMenu(e);
			}

			/**
			 * @param e
			 */
			private void showMenu(MouseEvent e) {
				final Object selObject = tree.getSelectedUserObject();

				if (selObject instanceof SearchListDTO)
					popup.show(e.getComponent(), e.getX(), e.getY());
			}
		});
	}

}
