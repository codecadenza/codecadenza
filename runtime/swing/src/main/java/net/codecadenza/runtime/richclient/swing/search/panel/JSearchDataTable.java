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
package net.codecadenza.runtime.richclient.swing.search.panel;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import javax.swing.event.TableColumnModelEvent;
import javax.swing.table.TableColumn;
import net.codecadenza.runtime.richclient.swing.widget.AbstractDataTableRenderer;
import net.codecadenza.runtime.richclient.swing.widget.ColumnInfo;
import net.codecadenza.runtime.richclient.swing.widget.JDataTable;
import net.codecadenza.runtime.search.dto.SearchDTO;
import net.codecadenza.runtime.search.dto.SearchFieldDTO;
import org.jdesktop.swingx.table.DefaultTableColumnModelExt;
import org.jdesktop.swingx.table.TableColumnExt;

/**
 * <p>
 * Data table component that supports generic input
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the table component
 */
public class JSearchDataTable<T> extends JDataTable<T> {
	private static final long serialVersionUID = 1827589034324985495L;

	private SearchDTO searchObj;
	protected HashMap<Integer, Integer> modelIndexMap;

	/**
	 * Constructor
	 * @param renderer
	 * @param sortKeyAccessor
	 */
	public JSearchDataTable(AbstractDataTableRenderer<T> renderer, SortKeyAccessor<T> sortKeyAccessor) {
		super(renderer, sortKeyAccessor);
	}

	/**
	 * The columns should be defined by a given search object
	 * @param searchObj
	 */
	@SuppressWarnings("unchecked")
	public void initColumns(SearchDTO searchObj) {
		final var headings = new ArrayList<String>();
		final List<SearchFieldDTO> fields = searchObj.getSearchFields();
		this.searchObj = searchObj;

		// While initializing the table we don't want to consume table column change events!
		initState = true;

		// Remove all existing columns
		final List<TableColumn> tableCols = getColumns(true);

		tableCols.forEach(this::removeColumn);

		fields.sort((o1, o2) -> o1.getColOrder() - o2.getColOrder());

		// Add all columns. We hide columns later if necessary!
		fields.forEach(field -> headings.add(field.getColLabel()));

		final var hs = new String[headings.size()];
		headings.toArray(hs);

		setColumns(hs);

		final DefaultTableColumnModelExt cols = getColumnModelAsDefault();
		int i = 0;

		for (final SearchFieldDTO field : fields) {
			final TableColumnExt col = cols.getColumnExt(i++);
			col.setPreferredWidth(field.getColWidth());
			col.setResizable(true);

			col.setComparator((o1, o2) -> {
				final int columnIndex = modelIndexMap.get(getSortedColumn().getModelIndex());
				final var val1 = (Comparable<Object>) sortKeyAccessor.getKey(columnIndex, (T) o1);
				final var val2 = (Comparable<Object>) sortKeyAccessor.getKey(columnIndex, (T) o2);

				if (val1 != null && val2 != null)
					return val1.compareTo(val2);
				else if (val1 == null && val2 != null)
					return -1;
				else if (val1 != null)
					return 1;
				else
					return 0;
			});
		}

		// Hide columns that shouldn't be visible! We have to do that in this way as getColumnExt() seems to have a problem!
		fields.forEach(field -> cols.getColumnExt(field.getColLabel()).setVisible(field.isVisible()));

		rebuildIndexMap();

		initState = false;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.widget.JDataTable#onColumnMoved(javax.swing.event.TableColumnModelEvent)
	 */
	@Override
	public void onColumnMoved(TableColumnModelEvent e) {
		super.onColumnMoved(e);

		if (e.getFromIndex() == e.getToIndex())
			return;

		for (final SearchFieldDTO firstField : getSearchObj().getSearchFields())
			if (firstField.getColOrder() == e.getFromIndex()) {
				boolean done = false;

				for (final SearchFieldDTO secondField : getSearchObj().getSearchFields())
					if (secondField.getColOrder() == e.getToIndex()) {
						firstField.setColOrder(e.getToIndex());
						secondField.setColOrder(e.getFromIndex());
						done = true;
						break;
					}

				if (done) {
					rebuildIndexMap();
					break;
				}
			}
	}

	/**
	 * @return the search object
	 */
	public SearchDTO getSearchObj() {
		return searchObj;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.widget.JDataTable#getColumnInfos()
	 */
	@Override
	public List<ColumnInfo> getColumnInfos() {
		final List<TableColumnExt> cols = getAllTableColumns();
		final var cs = new ColumnInfo[cols.size()];

		cols.forEach(col -> {
			// We have to search for the real visible index of the column. The model index is just a column ID!
			final int colOrderIndex = getSearchObj().getSearchFields().stream()
					.filter(field -> col.getTitle().equals(field.getColLabel())).findFirst().map(SearchFieldDTO::getColOrder).orElse(-1);

			// We use the model index as an ID and the colOrderIndex as visible column index
			final var colInfo = new ColumnInfo(col.getModelIndex(), col.isVisible(), colOrderIndex, col.getTitle());
			cs[col.getModelIndex()] = colInfo;
		});

		final List<ColumnInfo> list = Arrays.asList(cs);

		// Sort the columns based on the visible index
		list.sort((col1, col2) -> col1.getVisibleIndex() - col2.getVisibleIndex());

		return list;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.widget.JDataTable#rebuildIndexMap()
	 */
	@Override
	protected void rebuildIndexMap() {
		modelIndexMap = new HashMap<>();
		originalIndexMap = new HashMap<>();

		for (final ColumnInfo col : getColumnInfos()) {
			if (!col.isVisible())
				continue;

			for (final SearchFieldDTO field : getSearchObj().getSearchFields())
				if (col.getTitle().equals(field.getColLabel())) {
					// We need to know the mapping between the model index and original column index of the search field later on
					modelIndexMap.put(col.getIndex(), field.getOriginalColumnIndex());

					// Furthermore we save the mapping between the real column index and the original field index
					originalIndexMap.put(col.getVisibleIndex(), field.getOriginalColumnIndex());
				}
		}
	}

	/**
	 * @param searchObj
	 */
	public void setSearchObj(SearchDTO searchObj) {
		this.searchObj = searchObj;
	}

}
