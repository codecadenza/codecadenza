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
package net.codecadenza.runtime.richclient.swing.widget.dnd;

import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.io.IOException;
import java.lang.invoke.MethodHandles;
import java.util.Arrays;
import java.util.List;
import javax.swing.JComponent;
import javax.swing.JList;
import javax.swing.TransferHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * A {@link TransferHandler} that only supports transfers between lists
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ListTransferHandler extends TransferHandler {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static final long serialVersionUID = -740824723056368295L;
	public static final DataFlavor LIST_ITEM_FLAVOR = new DataFlavor(ListItemTransferable.class,
			ListItemTransferable.class.getSimpleName());

	// Contains all lists where data can be imported from
	private final List<JList<?>> acceptedSources;
	private final transient ITransferAction transferAction;

	/**
	 * Constructor
	 * @param transferAction
	 * @param acceptedSources
	 */
	public ListTransferHandler(ITransferAction transferAction, JList<?>... acceptedSources) {
		this.transferAction = transferAction;
		this.acceptedSources = Arrays.asList(acceptedSources);
	}

	/**
	 * Route all necessary information from the source list to the target list
	 */
	public static class ListItemTransferable implements Transferable {
		private final ITransferAction transferAction;
		private final JList<?> source;
		private final int[] selected;

		/**
		 * Constructor
		 * @param source
		 * @param transferAction
		 */
		public ListItemTransferable(JList<?> source, ITransferAction transferAction) {
			this.source = source;
			this.transferAction = transferAction;
			this.selected = source.getSelectedIndices();
		}

		/*
		 * (non-Javadoc)
		 * @see java.awt.datatransfer.Transferable#getTransferData(java.awt.datatransfer.DataFlavor)
		 */
		@Override
		public Object getTransferData(DataFlavor flavor) throws UnsupportedFlavorException, IOException {
			if (!isDataFlavorSupported(flavor))
				throw new IllegalArgumentException();

			return this;
		}

		/*
		 * (non-Javadoc)
		 * @see java.awt.datatransfer.Transferable#getTransferDataFlavors()
		 */
		@Override
		public DataFlavor[] getTransferDataFlavors() {
			return new DataFlavor[] { LIST_ITEM_FLAVOR };
		}

		/*
		 * (non-Javadoc)
		 * @see java.awt.datatransfer.Transferable#isDataFlavorSupported(java.awt.datatransfer.DataFlavor)
		 */
		@Override
		public boolean isDataFlavorSupported(DataFlavor flavor) {
			return Arrays.asList(getTransferDataFlavors()).contains(flavor);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see javax.swing.TransferHandler#canImport(javax.swing.TransferHandler.TransferSupport)
	 */
	@Override
	public boolean canImport(TransferSupport support) {
		if (!support.isDataFlavorSupported(LIST_ITEM_FLAVOR))
			return false;

		try {
			// Check if the target allows items to be dragged into it from source
			final var t = (ListItemTransferable) support.getTransferable().getTransferData(LIST_ITEM_FLAVOR);
			return acceptedSources.contains(t.source);
		}
		catch (final UnsupportedFlavorException _) {
			throw new AssertionError();
		}
		catch (final IOException _) {
			return false;
		}
	}

	/*
	 * (non-Javadoc)
	 * @see javax.swing.TransferHandler#importData(javax.swing.TransferHandler.TransferSupport)
	 */
	@Override
	public boolean importData(TransferSupport support) {
		if (!canImport(support))
			return false;

		try {
			final var t = (ListItemTransferable) support.getTransferable().getTransferData(LIST_ITEM_FLAVOR);
			final var target = (JList<?>) support.getComponent();
			final var info = new ListDragAndDropInfo(target, support, t.selected);

			// Execute the transfer action of the source in the current context
			return t.transferAction.executeAction(info);
		}
		catch (final Exception e) {
			logger.error("Error while performing drag and drop operation!", e);

			return false;
		}
	}

	/*
	 * (non-Javadoc)
	 * @see javax.swing.TransferHandler#createTransferable(javax.swing.JComponent)
	 */
	@Override
	protected Transferable createTransferable(JComponent c) {
		return new ListItemTransferable((JList<?>) c, transferAction);
	}

	/*
	 * (non-Javadoc)
	 * @see javax.swing.TransferHandler#getSourceActions(javax.swing.JComponent)
	 */
	@Override
	public int getSourceActions(JComponent c) {
		if (c instanceof JList)
			return COPY_OR_MOVE;

		return NONE;
	}

}
