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

import java.awt.Component;
import java.awt.dnd.DnDConstants;
import java.awt.dnd.DragGestureEvent;
import java.awt.dnd.DragGestureListener;
import java.awt.dnd.DragSource;
import java.awt.dnd.DragSourceDragEvent;
import java.awt.dnd.DragSourceDropEvent;
import java.awt.dnd.DragSourceEvent;
import java.awt.dnd.DragSourceListener;
import javax.swing.JTree;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreePath;

/**
 * <p>
 * Base class for all generated tree views
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JDataTree extends JTree {
	private static final long serialVersionUID = 8369630907188790697L;

	private DataTreeNode rootNode;

	/**
	 * Default drag handler for a tree
	 */
	private static class DragTreeHandler implements DragSourceListener, DragGestureListener {
		private final JDataTree tree;
		private final DragSource dragSourceTree;

		/**
		 * Constructor
		 * @param tree
		 */
		public DragTreeHandler(JDataTree tree) {
			this.tree = tree;
			this.dragSourceTree = DragSource.getDefaultDragSource();

			dragSourceTree.createDefaultDragGestureRecognizer(tree, DnDConstants.ACTION_COPY_OR_MOVE, this);
		}

		/*
		 * (non-Javadoc)
		 * @see java.awt.dnd.DragSourceListener#dragDropEnd(java.awt.dnd.DragSourceDropEvent)
		 */
		@Override
		public void dragDropEnd(DragSourceDropEvent dragSourceDropEvent) {
			// No implementation required!
		}

		/*
		 * (non-Javadoc)
		 * @see java.awt.dnd.DragSourceListener#dragEnter(java.awt.dnd.DragSourceDragEvent)
		 */
		@Override
		public void dragEnter(DragSourceDragEvent dragSourceDragEvent) {
			// No implementation required!
		}

		/*
		 * (non-Javadoc)
		 * @see java.awt.dnd.DragSourceListener#dragExit(java.awt.dnd.DragSourceEvent)
		 */
		@Override
		public void dragExit(DragSourceEvent dragSourceEvent) {
			// No implementation required!
		}

		/*
		 * (non-Javadoc)
		 * @see java.awt.dnd.DragSourceListener#dragOver(java.awt.dnd.DragSourceDragEvent)
		 */
		@Override
		public void dragOver(DragSourceDragEvent dragSourceDragEvent) {
			// No implementation required!
		}

		/*
		 * (non-Javadoc)
		 * @see java.awt.dnd.DragSourceListener#dropActionChanged(java.awt.dnd.DragSourceDragEvent)
		 */
		@Override
		public void dropActionChanged(DragSourceDragEvent dragSourceDragEvent) {
			// No implementation required!
		}

		/*
		 * (non-Javadoc)
		 * @see java.awt.dnd.DragGestureListener#dragGestureRecognized(java.awt.dnd.DragGestureEvent)
		 */
		@Override
		public void dragGestureRecognized(DragGestureEvent dge) {
			final TreePath path = tree.getSelectionPath();

			// The root node and an empty selection cannot be moved!
			if (path == null)
				return;

			final var node = (DataTreeNode) tree.getLastSelectedPathComponent();
			dragSourceTree.startDrag(dge, DragSource.DefaultMoveDrop, node, this);
		}
	}

	/**
	 * Constructor
	 * @param rootNode
	 */
	public JDataTree(DataTreeNode rootNode) {
		super(rootNode);
		this.rootNode = rootNode;

		this.setCellRenderer(new DefaultTreeCellRenderer() {
			private static final long serialVersionUID = -2561534472315289193L;

			/*
			 * (non-Javadoc)
			 * @see javax.swing.tree.DefaultTreeCellRenderer#getTreeCellRendererComponent(javax.swing.JTree, java.lang.Object, boolean,
			 * boolean, boolean, int, boolean)
			 */
			@Override
			public Component getTreeCellRendererComponent(JTree tree, Object value, boolean sel, boolean expanded, boolean leaf,
					int row, boolean hasFocus) {
				super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, hasFocus);

				final var node = (DataTreeNode) value;

				if (node.getIcon() != null)
					this.setIcon(node.getIcon());

				this.setText(node.getLabel());

				return this;
			}
		});

		new DragTreeHandler(this);
	}

	/**
	 * @return the associated user object of the selected node
	 */
	public Object getSelectedUserObject() {
		final DataTreeNode treeNode = getSelectedTreeNode();

		if (treeNode == null)
			return null;

		return treeNode.getUserObject();
	}

	/**
	 * @return the selected tree node
	 */
	public DataTreeNode getSelectedTreeNode() {
		final TreePath path = getSelectionPath();

		if (path == null)
			return null;

		return (DataTreeNode) path.getLastPathComponent();
	}

	/**
	 * Expand all nodes
	 */
	public void expandAll() {
		for (int row = 0; row < getRowCount(); row++)
			expandRow(row);
	}

	/**
	 * Collapse all nodes
	 */
	public void collapseAll() {
		for (int row = 0; row < getRowCount(); row++)
			collapseRow(row);
	}

	/**
	 * Expand the selected node
	 * @param node
	 */
	public void expandNode(DataTreeNode node) {
		expandRow(this.getRowForPath(new TreePath(node)));
	}

	/**
	 * Collapse the selected node
	 * @param node
	 */
	public void collapseNode(DataTreeNode node) {
		collapseRow(getRowForPath(new TreePath(node)));
	}

	/**
	 * Refresh the tree model
	 * @param rootNode
	 */
	public void refreshTreeModel(DataTreeNode rootNode) {
		this.rootNode = rootNode;
		setModel(new DefaultTreeModel(rootNode));
	}

	/**
	 * Add a node to the root node
	 * @param node
	 */
	public void addNodeToRoot(DataTreeNode node) {
		((DefaultTreeModel) getModel()).insertNodeInto(node, rootNode, rootNode.getChildCount());
	}

	/**
	 * Add a node to the parent node
	 * @param node
	 * @param parentNode
	 */
	public void addNodeToParent(DataTreeNode node, DataTreeNode parentNode) {
		if (parentNode == null) {
			addNodeToRoot(node);
			return;
		}

		((DefaultTreeModel) getModel()).insertNodeInto(node, parentNode, parentNode.getChildCount());
	}

	/**
	 * Refresh the model of a dedicated node
	 * @param node
	 */
	public void reloadNode(DataTreeNode node) {
		((DefaultTreeModel) getModel()).reload(node);
	}

	/**
	 * Remove a node from the tree
	 * @param node
	 */
	public void removeNode(DataTreeNode node) {
		final var parentItem = (DataTreeNode) node.getParent();

		if (parentItem != null) {
			node.removeFromParent();
			reloadNode(parentItem);
		}
		else
			node.removeFromParent();
	}

	/**
	 * Remove all child nodes from a parent node. The child node must reference a user object of the given type in order to be
	 * removed!
	 * @param parentItem
	 * @param cl
	 */
	public void removeChildNodes(DataTreeNode parentItem, Class<?> cl) {
		while (true) {
			// Remove existing items
			for (int i = 0; i < parentItem.getChildCount(); i++) {
				final var node = (DataTreeNode) parentItem.getChildAt(i);

				if (node.getUserObject() != null && cl.isInstance(node.getUserObject()))
					removeNode(node);
			}

			boolean nodesStillExist = false;

			for (int i = 0; i < parentItem.getChildCount(); i++) {
				final var node = (DataTreeNode) parentItem.getChildAt(i);

				if (node.getUserObject() != null && cl.isInstance(node.getUserObject())) {
					nodesStillExist = true;
					break;
				}
			}

			if (!nodesStillExist) {
				reloadNode(parentItem);
				break;
			}
		}
	}

}
