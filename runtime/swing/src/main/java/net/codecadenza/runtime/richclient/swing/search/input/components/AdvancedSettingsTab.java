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
package net.codecadenza.runtime.richclient.swing.search.input.components;

import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.ADVANCED_SETTINGS_TAB_CB_CASE_SENSITIVE;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.ADVANCED_SETTINGS_TAB_CB_COUNT_RECORDS;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.ADVANCED_SETTINGS_TAB_CB_EXACT_FILTER_MATCH;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.ADVANCED_SETTINGS_TAB_LBL_SORTING;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.ADVANCED_SETTINGS_TAB_MAX_FETCH_SIZE;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.ADVANCED_SETTINGS_TAB_TAB_ADV_SETTINGS;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.ADVANCED_SETTINGS_TAB_VISUAL_FIELDS;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.getTranslation;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.getTranslationForFieldLabel;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.Serializable;
import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import javax.swing.DefaultComboBoxModel;
import javax.swing.DefaultListCellRenderer;
import javax.swing.DefaultListModel;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.ListSelectionModel;
import javax.swing.border.LineBorder;
import javax.swing.border.TitledBorder;
import net.codecadenza.runtime.richclient.swing.image.ImageLoader;
import net.codecadenza.runtime.richclient.swing.widget.DualDataSelectionListPanel;
import net.codecadenza.runtime.richclient.swing.widget.dnd.ListDragAndDropInfo;
import net.codecadenza.runtime.richclient.swing.widget.dnd.ListTransferHandler;
import net.codecadenza.runtime.search.dto.SearchDTO;
import net.codecadenza.runtime.search.dto.SearchFieldDTO;
import net.codecadenza.runtime.search.dto.SearchFieldTypeEnum;
import net.codecadenza.runtime.search.dto.SortDirectionEnum;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Represents the tab folder where the user can select the visible columns
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class AdvancedSettingsTab implements Serializable {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static final long serialVersionUID = 674311938523181434L;

	// The minimum number of fields to show
	public static final int MIN_VISUAL_FIELD_COUNT = 2;

	private final JPanel pane;
	private final JCheckBox cbCaseSensitive;
	private final JCheckBox cbCountRecords;
	private final JCheckBox cbExactFilterMatch;
	private final JComboBox<Integer> cboMaxFetchSize;
	private final JList<SearchFieldDTO> lstSorting;
	private final SearchDTO searchObj;
	private DualDataSelectionListPanel<SearchFieldDTO> searchFieldSelector;

	/**
	 * Constructor
	 * @param searchObj
	 */
	public AdvancedSettingsTab(SearchDTO searchObj) {
		this.pane = new JPanel();

		cbCaseSensitive = new JCheckBox(getTranslation(ADVANCED_SETTINGS_TAB_CB_CASE_SENSITIVE));
		cbCountRecords = new JCheckBox(getTranslation(ADVANCED_SETTINGS_TAB_CB_COUNT_RECORDS));
		cbExactFilterMatch = new JCheckBox(getTranslation(ADVANCED_SETTINGS_TAB_CB_EXACT_FILTER_MATCH));
		lstSorting = new JList<>(new DefaultListModel<>());
		cboMaxFetchSize = new JComboBox<>();

		initFetchSize((DefaultComboBoxModel<Integer>) cboMaxFetchSize.getModel());

		this.searchObj = searchObj;
	}

	/**
	 * @param model
	 */
	protected void initFetchSize(DefaultComboBoxModel<Integer> model) {
		model.addElement(10);
		model.addElement(100);
		model.addElement(1000);
		model.addElement(10000);
		model.addElement(100_000);
		model.addElement(1_000_000);
	}

	/**
	 * Add the tab folder for the advanced search settings
	 * @param tabs
	 */
	public void addTabTo(JTabbedPane tabs) {
		tabs.addTab(getTranslation(ADVANCED_SETTINGS_TAB_TAB_ADV_SETTINGS), new JScrollPane(pane));

		pane.setLayout(new BorderLayout());

		final var optPane = new JPanel();

		final var layout = new GridBagLayout();
		layout.columnWidths = new int[] { 150, 150, 150 };
		layout.columnWeights = new double[] { 0, 0, 1 };

		optPane.setLayout(layout);

		final var cs = new GridBagConstraints();
		cs.anchor = GridBagConstraints.BASELINE_LEADING;

		cs.insets = new Insets(0, 3, 3, 0);

		optPane.add(cbCaseSensitive, cs);
		optPane.add(cbCountRecords, cs);
		optPane.add(cbExactFilterMatch, cs);

		cs.gridy = 1;

		optPane.add(new JLabel(getTranslationForFieldLabel(ADVANCED_SETTINGS_TAB_MAX_FETCH_SIZE)), cs);
		optPane.add(cboMaxFetchSize, cs);

		pane.add(optPane, BorderLayout.NORTH);

		final var columnsDetail = new JPanel();
		columnsDetail.setLayout(new BorderLayout(0, 3));
		columnsDetail.setBorder(
				new TitledBorder(LineBorder.createGrayLineBorder(), getTranslationForFieldLabel(ADVANCED_SETTINGS_TAB_VISUAL_FIELDS)));

		final var allFields = new ArrayList<SearchFieldDTO>();
		final var selectedFields = new ArrayList<SearchFieldDTO>();

		for (final SearchFieldDTO field : searchObj.getSearchFields()) {
			if (field.getType() == SearchFieldTypeEnum.NOT_SEARCHABLE)
				continue;

			allFields.add(field);

			if (field.isVisible())
				selectedFields.add(field);
		}

		searchFieldSelector = initListDragAndDrop(allFields);
		searchFieldSelector.setBorder(null);
		searchFieldSelector.setSelectedItems(selectedFields);

		columnsDetail.add(searchFieldSelector, BorderLayout.PAGE_START);

		final var lblSorting = new JLabel(getTranslationForFieldLabel(ADVANCED_SETTINGS_TAB_LBL_SORTING));

		columnsDetail.add(lblSorting, BorderLayout.CENTER);

		lstSorting.setCellRenderer(new SortOrderRenderer());

		columnsDetail.add(new JScrollPane(lstSorting), BorderLayout.PAGE_END);

		initSortList();

		cbCaseSensitive.setSelected(searchObj.isCaseSensitive());
		cbCountRecords.setSelected(searchObj.isCount());
		cbExactFilterMatch.setSelected(searchObj.isExactFilterMatch());
		cboMaxFetchSize.setSelectedItem(searchObj.getMaxResult());

		pane.add(columnsDetail, BorderLayout.SOUTH);
	}

	/**
	 * @param allFields
	 * @return the created selection panel
	 */
	private DualDataSelectionListPanel<SearchFieldDTO> initListDragAndDrop(final List<SearchFieldDTO> allFields) {
		final var selectionPanel = new DualDataSelectionListPanel<SearchFieldDTO>(false, false) {
			private static final long serialVersionUID = 3324496446325719235L;

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.DualDataSelectionListPanel#getItemText(java.lang.Object)
			 */
			@Override
			public String getItemText(SearchFieldDTO element) {
				return element.getColLabel();
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.DualDataSelectionListPanel#searchItems(java.lang.String)
			 */
			@Override
			public List<SearchFieldDTO> searchItems(String filter) {
				return allFields;
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.DualDataSelectionListPanel#moveFromTarget()
			 */
			@Override
			public void moveFromTarget() {
				// Only move if there are enough elements
				if (getTargetList().getModel().getSize() > MIN_VISUAL_FIELD_COUNT)
					super.moveFromTarget();
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.DualDataSelectionListPanel#moveAllFromTarget()
			 */
			@Override
			public void moveAllFromTarget() {
				final List<SearchFieldDTO> selected = getSelectedItems();

				// Only move if there are enough elements
				if (selected.size() > MIN_VISUAL_FIELD_COUNT) {
					final List<SearchFieldDTO> toMove = selected.subList(MIN_VISUAL_FIELD_COUNT, selected.size());

					getAvailableItems().addAll(toMove);
					toMove.clear();

					updatePresentation();
				}
			}
		};

		lstSorting.setDragEnabled(true);
		lstSorting.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);

		// Allow elements to be dropped from itself (to shift elements) or from the target
		lstSorting.setTransferHandler(new ListTransferHandler(info -> {
			final SearchFieldDTO selected = ((DefaultListModel<SearchFieldDTO>) lstSorting.getModel())
					.get(info.getSelectedIndices()[0]);

			if (selected == null)
				return false;

			moveElement(info, selected);
			return true;
		}, selectionPanel.getTargetList(), lstSorting));

		selectionPanel.getSourceList().setDragEnabled(true);
		selectionPanel.getTargetList().setDragEnabled(true);

		// Allow items to be dropped from the target
		selectionPanel.getSourceList().setTransferHandler(new ListTransferHandler(_ -> {
			selectionPanel.moveFromSource();
			return true;
		}, selectionPanel.getTargetList()));

		// Allow elements to be dropped from the source
		selectionPanel.getTargetList().setTransferHandler(new ListTransferHandler(info -> {
			if (info.getTarget() == selectionPanel.getSourceList()) {
				selectionPanel.moveFromTarget();
				return true;
			}

			final int selIndex = selectionPanel.getTargetList().getSelectedIndex();

			if (selIndex < 0)
				return false;

			final SearchFieldDTO field = selectionPanel.getSelectedItems().get(selIndex);
			field.setSortOrder(SortDirectionEnum.ASC);

			moveElement(info, field);
			return true;
		}, selectionPanel.getSourceList()));

		// Invert the sort order when performing a double-click
		lstSorting.addMouseListener(new MouseAdapter() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.MouseAdapter#mouseClicked(java.awt.event.MouseEvent)
			 */
			@Override
			public void mouseClicked(MouseEvent e) {
				if (!e.isConsumed() && e.getClickCount() == 2) {
					e.consume();

					final int index = lstSorting.locationToIndex(e.getPoint());
					final var model = (DefaultListModel<SearchFieldDTO>) lstSorting.getModel();

					final SearchFieldDTO clicked = model.get(index);
					clicked.setSortOrder(invertSortOrder(clicked.getSortOrder()));

					// Notify the view
					model.setElementAt(clicked, index);
				}
			}
		});

		// If DELETE is pressed, remove the selected element
		lstSorting.addKeyListener(new KeyAdapter() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.KeyAdapter#keyPressed(java.awt.event.KeyEvent)
			 */
			@Override
			public void keyPressed(KeyEvent e) {
				if (!e.isConsumed() && e.getKeyCode() == KeyEvent.VK_DELETE) {
					e.consume();

					final var model = (DefaultListModel<SearchFieldDTO>) lstSorting.getModel();
					final SearchFieldDTO selected = lstSorting.getSelectedValue();

					if (selected != null) {
						selected.setSortIndex(0);
						selected.setSortOrder(SortDirectionEnum.NONE);

						model.removeElement(selected);
					}
				}
			}
		});

		// If the items are removed from the list of selected items, remove them also from the sorting list
		selectionPanel.getTargetList().addPropertyChangeListener("model", _ -> {
			final var model = (DefaultListModel<SearchFieldDTO>) lstSorting.getModel();

			for (int i = model.size() - 1; i >= 0; --i)
				if (!selectionPanel.getSelectedItems().contains(model.elementAt(i))) {
					final SearchFieldDTO field = model.getElementAt(i);
					field.setSortIndex(-1);
					field.setSortOrder(SortDirectionEnum.NONE);

					model.remove(i);
				}
		});

		return selectionPanel;
	}

	/**
	 * Invert the sort order
	 * @param sortOrder
	 * @return the new sort order
	 */
	protected SortDirectionEnum invertSortOrder(SortDirectionEnum sortOrder) {
		return switch (sortOrder) {
			case NONE -> SortDirectionEnum.NONE;
			case ASC -> SortDirectionEnum.DESC;
			case DESC -> SortDirectionEnum.ASC;
			default -> throw new AssertionError("unmatched case");
		};
	}

	/**
	 * Move the element
	 * @param info
	 * @param elem
	 */
	@SuppressWarnings("unchecked")
	private static void moveElement(ListDragAndDropInfo info, SearchFieldDTO elem) {
		try {
			final var model = (DefaultListModel<SearchFieldDTO>) info.getTarget().getModel();
			int dropLoc = ((JList.DropLocation) info.getSupport().getDropLocation()).getIndex();
			final int cur = model.indexOf(elem);

			if (cur >= 0) {
				model.remove(cur);

				if (dropLoc >= 0 && dropLoc >= cur)
					--dropLoc;
			}

			if (dropLoc < 0)
				model.addElement(elem);
			else
				model.insertElementAt(elem, dropLoc);
		}
		catch (final Exception e) {
			logger.error("Error while moving column by drag and drop!", e);
		}
	}

	/**
	 * Reset the selection
	 */
	public void clear() {
		final var model = (DefaultListModel<SearchFieldDTO>) lstSorting.getModel();
		model.clear();

		searchObj.getSearchFields().forEach(field -> {
			field.setVisible(true);
			field.setSortIndex(0);
			field.setSortOrder(SortDirectionEnum.NONE);
		});

		searchFieldSelector.moveAllFromSource();
	}

	/**
	 * Initialize the sort list
	 */
	private void initSortList() {
		// Sort the search fields in reverse sort order! The sort order index 0 has the highest priority!
		searchObj.getSearchFields().sort((o1, o2) -> -(o2.getSortIndex() - o1.getSortIndex()));

		// Add all appropriate search fields to the sort list
		final var sortFields = (DefaultListModel<SearchFieldDTO>) lstSorting.getModel();

		for (final SearchFieldDTO field : searchObj.getSearchFields())
			if (field.getSortOrder() != SortDirectionEnum.NONE)
				sortFields.addElement(field);
	}

	/**
	 * Write the configuration to the search object
	 */
	public void validateAndSet() {
		int maxColOrderIndex = 0;
		int colOrder = 0;

		searchObj.setCaseSensitive(cbCaseSensitive.isSelected());
		searchObj.setCount(cbCountRecords.isSelected());
		searchObj.setExactFilterMatch(cbExactFilterMatch.isSelected());
		searchObj.setMaxResult((Integer) cboMaxFetchSize.getSelectedItem());

		// Hide invisible fields
		searchFieldSelector.getAvailableItems().forEach(field -> {
			field.setVisible(false);
			field.setColOrder(-1);
		});

		// Search for the max. column order index
		for (final SearchFieldDTO field : searchObj.getSearchFields())
			if (field.getColOrder() > maxColOrderIndex)
				maxColOrderIndex = field.getColOrder();

		// Show all visible fields and set the column order
		for (final SearchFieldDTO field : searchFieldSelector.getSelectedItems()) {
			field.setVisible(true);

			if (field.getColOrder() == -1)
				field.setColOrder(++maxColOrderIndex);
		}

		// Sort fields and reorganize the column order
		searchObj.getSearchFields().sort((o1, o2) -> o1.getColOrder() - o2.getColOrder());

		for (final SearchFieldDTO field : searchObj.getSearchFields()) {
			if (!field.isVisible())
				continue;

			field.setColOrder(colOrder++);
		}

		final var sortFields = (DefaultListModel<SearchFieldDTO>) lstSorting.getModel();

		int i = 0;

		for (final SearchFieldDTO field : Collections.list(sortFields.elements()))
			field.setSortIndex(i++);
	}

	/**
	 * Sort order renderer
	 */
	public static class SortOrderRenderer extends DefaultListCellRenderer {
		private static final long serialVersionUID = 7039868471491120328L;

		/*
		 * (non-Javadoc)
		 * @see javax.swing.DefaultListCellRenderer#getListCellRendererComponent(javax.swing.JList, java.lang.Object, int, boolean,
		 * boolean)
		 */
		@Override
		public Component getListCellRendererComponent(JList<?> list, Object value, int index, boolean isSelected,
				boolean cellHasFocus) {
			final var lbl = (JLabel) super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
			final var field = (SearchFieldDTO) value;

			lbl.setText(field.getColLabel());
			lbl.setIcon(ImageLoader.getImage(sortOrderImagePath(field.getSortOrder())));

			return lbl;
		}

		/**
		 * @param sortOrder
		 * @return the sort order image path
		 */
		private static String sortOrderImagePath(SortDirectionEnum sortOrder) {
			return switch (sortOrder) {
				case ASC -> ImageLoader.SORT_ASC;
				case DESC -> ImageLoader.SORT_DESC;
				default -> throw new AssertionError("unmatched case");
			};
		}
	}

}
