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

import javax.swing.JList;
import javax.swing.TransferHandler.TransferSupport;

/**
 * <p>
 * Helper class that holds information about a drag-and-drop transfer
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ListDragAndDropInfo {
	private JList<?> target;
	private TransferSupport support;
	private int[] selectedIndices;

	/**
	 * Constructor
	 * @param target
	 * @param support
	 * @param selectedIndices
	 */
	public ListDragAndDropInfo(JList<?> target, TransferSupport support, int[] selectedIndices) {
		this.target = target;
		this.support = support;
		this.selectedIndices = selectedIndices;
	}

	/**
	 * @return the target list
	 */
	public JList<?> getTarget() {
		return target;
	}

	/**
	 * @param target
	 */
	public void setTarget(JList<?> target) {
		this.target = target;
	}

	/**
	 * @return the transfer support
	 */
	public TransferSupport getSupport() {
		return support;
	}

	/**
	 * @param support
	 */
	public void setSupport(TransferSupport support) {
		this.support = support;
	}

	/**
	 * @return an array of selected items
	 */
	public int[] getSelectedIndices() {
		return selectedIndices;
	}

	/**
	 * @param selectedIndices
	 */
	public void setSelectedIndices(int[] selectedIndices) {
		this.selectedIndices = selectedIndices;
	}

}
