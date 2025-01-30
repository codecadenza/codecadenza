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

import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.io.IOException;
import javax.swing.Icon;
import javax.swing.tree.DefaultMutableTreeNode;

/**
 * <p>
 * Simple improvement of a tree node
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DataTreeNode extends DefaultMutableTreeNode implements Transferable {
	public static final transient DataFlavor NODE_FLAVOR = new DataFlavor(DataTreeNode.class, DataTreeNode.class.getName());
	private static final long serialVersionUID = 5632720201662134994L;

	private transient Icon icon;
	private transient String label;
	private transient DataFlavor[] flavors = { NODE_FLAVOR };
	private transient AbstractSubItemHelperDTO helperObject;

	/**
	 * @return the image icon of the node
	 */
	public Icon getIcon() {
		return icon;
	}

	/**
	 * @param icon
	 */
	public void setIcon(Icon icon) {
		this.icon = icon;
	}

	/**
	 * @param label
	 */
	public void setLabel(String label) {
		this.label = label;
	}

	/**
	 * @return the label to be displayed
	 */
	public String getLabel() {
		return label;
	}

	/**
	 * @return true if data for this node is loaded
	 */
	public boolean isDataLoaded() {
		if (helperObject == null)
			return false;

		return helperObject.isDataLoaded();
	}

	/**
	 * @param loaded
	 */
	public void setDataLoaded(boolean loaded) {
		if (helperObject == null)
			return;

		helperObject.setDataLoaded(loaded);
	}

	/**
	 * @return the node's helper object
	 */
	public AbstractSubItemHelperDTO getHelperObject() {
		return helperObject;
	}

	/**
	 * @param helperObject
	 */
	public void setHelperObject(AbstractSubItemHelperDTO helperObject) {
		this.helperObject = helperObject;
	}

	/**
	 * Constructor
	 * @param parent
	 * @param label
	 * @param icon
	 */
	public DataTreeNode(DataTreeNode parent, String label, Icon icon) {
		this(label, icon);

		if (parent != null)
			parent.add(this);
	}

	/**
	 * Constructor
	 * @param label
	 * @param icon
	 */
	public DataTreeNode(String label, Icon icon) {
		this.icon = icon;
		this.label = label;
	}

	/**
	 * Constructor
	 * @param label
	 * @param icon
	 * @param userObject
	 */
	public DataTreeNode(String label, Icon icon, Object userObject) {
		this(label, icon);

		this.setUserObject(userObject);
	}

	/**
	 * Constructor
	 * @param parent
	 * @param label
	 * @param icon
	 * @param userObject
	 */
	public DataTreeNode(DataTreeNode parent, String label, Icon icon, Object userObject) {
		this(parent, label, icon);

		this.setUserObject(userObject);
	}

	/**
	 * Constructor
	 * @param parent
	 * @param label
	 * @param icon
	 * @param userObject
	 * @param helperObject
	 */
	public DataTreeNode(DataTreeNode parent, String label, Icon icon, Object userObject, AbstractSubItemHelperDTO helperObject) {
		this(parent, label, icon, userObject);
		this.helperObject = helperObject;
	}

	/*
	 * (non-Javadoc)
	 * @see java.awt.datatransfer.Transferable#getTransferData(java.awt.datatransfer.DataFlavor)
	 */
	@Override
	public Object getTransferData(DataFlavor flavor) throws UnsupportedFlavorException, IOException {
		if (isDataFlavorSupported(flavor))
			return this;

		throw new UnsupportedFlavorException(flavor);
	}

	/*
	 * (non-Javadoc)
	 * @see java.awt.datatransfer.Transferable#getTransferDataFlavors()
	 */
	@Override
	public DataFlavor[] getTransferDataFlavors() {
		return flavors;
	}

	/*
	 * (non-Javadoc)
	 * @see java.awt.datatransfer.Transferable#isDataFlavorSupported(java.awt.datatransfer.DataFlavor)
	 */
	@Override
	public boolean isDataFlavorSupported(DataFlavor flavor) {
		return (flavor.getRepresentationClass() == DataTreeNode.class);
	}

}
