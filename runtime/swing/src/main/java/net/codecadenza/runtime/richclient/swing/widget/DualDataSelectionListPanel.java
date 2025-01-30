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

import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.DUAL_DATA_SELECTION_LIST_PANEL_CMD_DESELECT_ALL;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.DUAL_DATA_SELECTION_LIST_PANEL_CMD_DESELECT_ITEM;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.DUAL_DATA_SELECTION_LIST_PANEL_CMD_SEARCH;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.DUAL_DATA_SELECTION_LIST_PANEL_CMD_SELECT_ALL;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.DUAL_DATA_SELECTION_LIST_PANEL_CMD_SELECT_ITEM;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.DUAL_DATA_SELECTION_LIST_PANEL_LBL_SEARCH_ELEMENTS;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.DUAL_DATA_SELECTION_LIST_PANEL_LBL_SOURCE_LIST;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.DUAL_DATA_SELECTION_LIST_PANEL_LBL_TARGET_LIST;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.getTranslation;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.getTranslationForFieldLabel;

import java.awt.BorderLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JRootPane;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.border.EmptyBorder;
import net.codecadenza.runtime.richclient.swing.image.ImageLoader;

/**
 * <p>
 * Generic panel for selecting elements of a list
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the selection list
 */
public abstract class DualDataSelectionListPanel<T> extends JPanel {
	private static final long serialVersionUID = -3036843474729092872L;

	private JTextField txtFilter;
	protected JList<String> listSource;
	protected JList<String> listTarget;
	private JRootPane rootPane;
	private JButton defaultButton;
	private boolean readonly;

	/**
	 * The items in the left list (source)
	 */
	protected transient List<T> availableElements = new ArrayList<>();

	/**
	 * The items in the right list (target)
	 */
	protected transient List<T> selectedElements = new ArrayList<>();

	/**
	 * Get the item text. This method is intended to be overwritten by subclasses!
	 * @param element
	 * @return the item text
	 */
	public abstract String getItemText(T element);

	/**
	 * @param filter text
	 * @return the items found
	 */
	public abstract List<T> searchItems(String filter);

	/**
	 * Refresh the list that contains the available items
	 * @param filter
	 */
	protected void refreshSourceList(String filter) {
		if (filter != null && filter.isEmpty())
			availableElements = Collections.emptyList();
		else
			availableElements = getValidItems(searchItems(filter));

		listSource.setListData(convertToDisplay(availableElements));
	}

	/**
	 * @param selItems
	 */
	public final void setSelectedItems(Collection<T> selItems) {
		setSelectedItems(new ArrayList<>(selItems));
	}

	/**
	 * Set the selected items
	 * @param selItems
	 */
	public final void setSelectedItems(List<T> selItems) {
		selectedElements = selItems;

		for (final Iterator<T> iter = availableElements.iterator(); iter.hasNext();)
			if (selectedElements.contains(iter.next()))
				iter.remove();

		updatePresentation();
	}

	/**
	 * @return a list containing all selected elements
	 */
	public final List<T> getSelectedItems() {
		return selectedElements;
	}

	/**
	 * @return a list containing all unselected elements
	 */
	public final List<T> getAvailableItems() {
		return availableElements;
	}

	/**
	 * Add items of the search result only if they haven't been selected yet!
	 * @param searchItems
	 * @return a list containing all valid items
	 */
	public final List<T> getValidItems(List<T> searchItems) {
		final var resultList = new ArrayList<T>();

		for (final T item : searchItems)
			if (!selectedElements.contains(item))
				resultList.add(item);

		return resultList;
	}

	/**
	 * Compare the elements in the lists. A subclass may overwrite this method.
	 * @param obj1
	 * @param obj2
	 * @return the result of the compare operation
	 */
	@SuppressWarnings("unused")
	public int compareElements(T obj1, T obj2) {
		return 0;
	}

	/**
	 * Convert the data model to an array of items that should be displayed in the list
	 * @param selItems
	 * @return an array of strings to be displayed in one of the list components
	 */
	private String[] convertToDisplay(List<T> selItems) {
		int i = 0;

		selItems.sort(this::compareElements);

		final var display = new String[selItems.size()];

		for (final T item : selItems)
			display[i++] = getItemText(item);

		return display;
	}

	/**
	 * Create the panel
	 * @param enableSearch a flag that determines if manual search by filter criteria should be enabled
	 * @param readonly
	 */
	protected DualDataSelectionListPanel(boolean enableSearch, boolean readonly) {
		this.readonly = readonly;

		final var gridBagLayout = new GridBagLayout();

		if (enableSearch && !readonly) {
			gridBagLayout.columnWidths = new int[] { 200, 50, 200 };
			gridBagLayout.rowHeights = new int[] { 23, 200, 0 };
			gridBagLayout.columnWeights = new double[] { 1.0, 0.0, 1.0 };
			gridBagLayout.rowWeights = new double[] { 0.0, 1.0, Double.MIN_VALUE };
		}
		else {
			gridBagLayout.columnWidths = new int[] { 200, 50, 200 };
			gridBagLayout.rowHeights = new int[] { 200, 0 };
			gridBagLayout.columnWeights = new double[] { 1.0, 0.0, 1.0 };
			gridBagLayout.rowWeights = new double[] { 1.0, Double.MIN_VALUE };
		}

		setLayout(gridBagLayout);

		if (enableSearch && !readonly) {
			final var panSearchElements = new JPanel();
			panSearchElements.setBorder(new EmptyBorder(0, 5, 0, 0));

			final var gbcSearchElements = new GridBagConstraints();
			gbcSearchElements.anchor = GridBagConstraints.NORTH;
			gbcSearchElements.fill = GridBagConstraints.HORIZONTAL;
			gbcSearchElements.insets = new Insets(0, 0, 5, 0);
			gbcSearchElements.gridwidth = 3;
			gbcSearchElements.gridx = 0;
			gbcSearchElements.gridy = 0;

			add(panSearchElements, gbcSearchElements);

			final var gblSearchElements = new GridBagLayout();
			gblSearchElements.columnWidths = new int[] { 108, 116, 86, 0, 0 };
			gblSearchElements.rowHeights = new int[] { 20, 0 };
			gblSearchElements.columnWeights = new double[] { 0.0, 0.0, 0.0, 0.0, Double.MIN_VALUE };
			gblSearchElements.rowWeights = new double[] { 0.0, Double.MIN_VALUE };

			panSearchElements.setLayout(gblSearchElements);

			final var lblSearchElements = new JLabel(getTranslationForFieldLabel(DUAL_DATA_SELECTION_LIST_PANEL_LBL_SEARCH_ELEMENTS));

			final var gbdLabelSearchElements = new GridBagConstraints();
			gbdLabelSearchElements.anchor = GridBagConstraints.WEST;
			gbdLabelSearchElements.insets = new Insets(0, 0, 0, 5);
			gbdLabelSearchElements.gridx = 0;
			gbdLabelSearchElements.gridy = 0;

			panSearchElements.add(lblSearchElements, gbdLabelSearchElements);

			txtFilter = new JTextField();

			txtFilter.addKeyListener(new KeyAdapter() {
				/*
				 * (non-Javadoc)
				 * @see java.awt.event.KeyAdapter#keyReleased(java.awt.event.KeyEvent)
				 */
				@Override
				public void keyReleased(KeyEvent e) {
					refreshSourceList(txtFilter.getText());
				}
			});

			txtFilter.addFocusListener(new FocusListener() {
				/*
				 * (non-Javadoc)
				 * @see java.awt.event.FocusListener#focusLost(java.awt.event.FocusEvent)
				 */
				@Override
				public void focusLost(FocusEvent arg0) {
					if (rootPane != null)
						rootPane.setDefaultButton(defaultButton);
				}

				/*
				 * (non-Javadoc)
				 * @see java.awt.event.FocusListener#focusGained(java.awt.event.FocusEvent)
				 */
				@Override
				public void focusGained(FocusEvent arg0) {
					if (DualDataSelectionListPanel.this.getParent() == null)
						return;

					if (DualDataSelectionListPanel.this.getParent() instanceof final JComponent parentComponent) {
						rootPane = parentComponent.getRootPane();
						defaultButton = rootPane.getDefaultButton();
						rootPane.setDefaultButton(null);
					}
				}
			});

			final var gbcFilter = new GridBagConstraints();
			gbcFilter.gridwidth = 2;
			gbcFilter.fill = GridBagConstraints.HORIZONTAL;
			gbcFilter.insets = new Insets(0, 0, 0, 5);
			gbcFilter.gridx = 1;
			gbcFilter.gridy = 0;

			panSearchElements.add(txtFilter, gbcFilter);

			txtFilter.setColumns(10);

			final var cmdSearch = new JButton(getTranslation(DUAL_DATA_SELECTION_LIST_PANEL_CMD_SEARCH));
			cmdSearch.addActionListener(action -> refreshSourceList(txtFilter.getText()));

			final var gbcButtonSearch = new GridBagConstraints();
			gbcButtonSearch.gridx = 3;
			gbcButtonSearch.gridy = 0;

			panSearchElements.add(cmdSearch, gbcButtonSearch);
		}

		final var panSource = new JPanel();
		panSource.setBorder(new EmptyBorder(5, 5, 5, 0));

		final var gbcPanelSource = new GridBagConstraints();
		gbcPanelSource.fill = GridBagConstraints.BOTH;
		gbcPanelSource.insets = new Insets(0, 0, 0, 5);
		gbcPanelSource.gridx = 0;

		if (enableSearch && !readonly)
			gbcPanelSource.gridy = 1;
		else
			gbcPanelSource.gridy = 0;

		add(panSource, gbcPanelSource);

		panSource.setLayout(new BorderLayout(0, 0));

		final var lblSourceList = new JLabel(getTranslationForFieldLabel(DUAL_DATA_SELECTION_LIST_PANEL_LBL_SOURCE_LIST));
		panSource.add(lblSourceList, BorderLayout.NORTH);

		final var scrPaneSource = new JScrollPane();
		panSource.add(scrPaneSource, BorderLayout.CENTER);

		listSource = new JList<>();

		if (!readonly)
			listSource.addMouseListener(new MouseAdapter() {
				/*
				 * (non-Javadoc)
				 * @see java.awt.event.MouseAdapter#mouseClicked(java.awt.event.MouseEvent)
				 */
				@Override
				public void mouseClicked(MouseEvent e) {
					if (e.getClickCount() == 2) {
						e.consume();
						moveFromSource();
					}
				}
			});

		scrPaneSource.setViewportView(listSource);

		final var panButtons = new JPanel();

		final var gbcPanelButtons = new GridBagConstraints();
		gbcPanelButtons.insets = new Insets(0, 0, 0, 5);
		gbcPanelButtons.gridx = 1;

		if (enableSearch && !readonly)
			gbcPanelButtons.gridy = 1;
		else
			gbcPanelButtons.gridy = 0;

		add(panButtons, gbcPanelButtons);

		final var gbl_panButtons = new GridBagLayout();
		gbl_panButtons.columnWidths = new int[] { 0, 0 };
		gbl_panButtons.rowHeights = new int[] { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
		gbl_panButtons.columnWeights = new double[] { 0.0, Double.MIN_VALUE };
		gbl_panButtons.rowWeights = new double[] { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, Double.MIN_VALUE };

		panButtons.setLayout(gbl_panButtons);

		final var cmdSelectAll = new JButton("");
		cmdSelectAll.setToolTipText(getTranslation(DUAL_DATA_SELECTION_LIST_PANEL_CMD_SELECT_ALL));
		cmdSelectAll.setIcon(ImageLoader.getImage(ImageLoader.SELECT_ALL));
		cmdSelectAll.setEnabled(!readonly);
		cmdSelectAll.addActionListener(action -> moveAllFromSource());

		final var gbcSelectAll = new GridBagConstraints();
		gbcSelectAll.insets = new Insets(0, 0, 5, 0);
		gbcSelectAll.gridx = 0;
		gbcSelectAll.gridy = 3;

		panButtons.add(cmdSelectAll, gbcSelectAll);

		final var cmdSelectItem = new JButton("");
		cmdSelectItem.setToolTipText(getTranslation(DUAL_DATA_SELECTION_LIST_PANEL_CMD_SELECT_ITEM));
		cmdSelectItem.setIcon(ImageLoader.getImage(ImageLoader.SELECT_ITEM));
		cmdSelectItem.setEnabled(!readonly);
		cmdSelectItem.addActionListener(action -> moveFromSource());

		final var gbcSelectItem = new GridBagConstraints();
		gbcSelectItem.insets = new Insets(0, 0, 5, 0);
		gbcSelectItem.gridx = 0;
		gbcSelectItem.gridy = 4;

		panButtons.add(cmdSelectItem, gbcSelectItem);

		final var cmdDeselectItem = new JButton("");
		cmdDeselectItem.setToolTipText(getTranslation(DUAL_DATA_SELECTION_LIST_PANEL_CMD_DESELECT_ITEM));
		cmdDeselectItem.setIcon(ImageLoader.getImage(ImageLoader.DESELECT_ITEM));
		cmdDeselectItem.setEnabled(!readonly);
		cmdDeselectItem.addActionListener(action -> moveFromTarget());

		final var gbcDeselectItem = new GridBagConstraints();
		gbcDeselectItem.insets = new Insets(0, 0, 5, 0);
		gbcDeselectItem.gridx = 0;
		gbcDeselectItem.gridy = 5;

		panButtons.add(cmdDeselectItem, gbcDeselectItem);

		final var cmdDeselectAll = new JButton("");
		cmdDeselectAll.setToolTipText(getTranslation(DUAL_DATA_SELECTION_LIST_PANEL_CMD_DESELECT_ALL));
		cmdDeselectAll.setIcon(ImageLoader.getImage(ImageLoader.DESELECT_ALL));
		cmdDeselectAll.setEnabled(!readonly);
		cmdDeselectAll.addActionListener(action -> moveAllFromTarget());

		final var gbcDeselectAll = new GridBagConstraints();
		gbcDeselectAll.insets = new Insets(0, 0, 5, 0);
		gbcDeselectAll.gridx = 0;
		gbcDeselectAll.gridy = 6;

		panButtons.add(cmdDeselectAll, gbcDeselectAll);

		final var panTarget = new JPanel();
		panTarget.setBorder(new EmptyBorder(5, 0, 5, 5));

		final var gbcPanelTarget = new GridBagConstraints();
		gbcPanelTarget.fill = GridBagConstraints.BOTH;
		gbcPanelTarget.gridx = 2;

		if (enableSearch && !readonly)
			gbcPanelTarget.gridy = 1;
		else
			gbcPanelTarget.gridy = 0;

		add(panTarget, gbcPanelTarget);

		panTarget.setLayout(new BorderLayout(0, 0));

		final var lblTargetList = new JLabel(getTranslationForFieldLabel(DUAL_DATA_SELECTION_LIST_PANEL_LBL_TARGET_LIST));
		panTarget.add(lblTargetList, BorderLayout.NORTH);

		final var scrPaneTarget = new JScrollPane();
		panTarget.add(scrPaneTarget, BorderLayout.CENTER);

		listTarget = new JList<>();
		listTarget.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);

		if (!readonly)
			listTarget.addMouseListener(new MouseAdapter() {
				/*
				 * (non-Javadoc)
				 * @see java.awt.event.MouseAdapter#mouseClicked(java.awt.event.MouseEvent)
				 */
				@Override
				public void mouseClicked(MouseEvent e) {
					if (e.getClickCount() == 2) {
						e.consume();
						moveFromTarget();
					}
				}
			});

		scrPaneTarget.setViewportView(listTarget);

		if (!enableSearch && !readonly)
			refreshSourceList(null);
	}

	/**
	 * Remove an element from the from-list and add it to the to-list and update the presentation
	 * @param i the index of the element to move
	 * @param from
	 * @param to
	 */
	protected void moveBetween(int i, List<T> from, List<T> to) {
		to.add(from.remove(i));

		updatePresentation();
	}

	/**
	 * Move all selected items from the source list to the target list
	 */
	public void moveFromSource() {
		if (readonly)
			return;

		final int[] selIndices = listSource.getSelectedIndices();

		for (int i = selIndices.length - 1; i >= 0; --i)
			selectedElements.add(availableElements.remove(selIndices[i]));

		updatePresentation();

		// If only one item was selected, select the item behind the selection
		if (selIndices.length == 1 && selIndices[0] < availableElements.size())
			listSource.setSelectedIndex(selIndices[0]);
	}

	/**
	 * Move all elements from the source list to the target list
	 */
	public void moveAllFromSource() {
		if (readonly)
			return;

		selectedElements.addAll(availableElements);
		availableElements.clear();

		updatePresentation();
	}

	/**
	 * Move the selected item from the target list to the source list
	 */
	public void moveFromTarget() {
		if (readonly)
			return;

		final int selIndex = listTarget.getSelectedIndex();

		if (selIndex == -1)
			return;

		moveBetween(selIndex, selectedElements, availableElements);

		if (selIndex < selectedElements.size())
			listTarget.setSelectedIndex(selIndex);
	}

	/**
	 * Move all items from the target list to the source list
	 */
	public void moveAllFromTarget() {
		if (readonly)
			return;

		availableElements.addAll(selectedElements);
		selectedElements.clear();

		updatePresentation();
	}

	/**
	 * Synchronize the presentation lists with their element lists
	 */
	public void updatePresentation() {
		listSource.setListData(convertToDisplay(availableElements));
		listTarget.setListData(convertToDisplay(selectedElements));
	}

	/**
	 * @return the source list
	 */
	public JList<String> getSourceList() {
		return listSource;
	}

	/**
	 * @return the target list
	 */
	public JList<String> getTargetList() {
		return listTarget;
	}

}
