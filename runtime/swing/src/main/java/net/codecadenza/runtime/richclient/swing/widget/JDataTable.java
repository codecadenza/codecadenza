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

import java.awt.BorderLayout;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.StringSelection;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.lang.invoke.MethodHandles;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;
import javax.swing.DefaultRowSorter;
import javax.swing.JComponent;
import javax.swing.JScrollPane;
import javax.swing.ListSelectionModel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.TableColumnModelEvent;
import javax.swing.event.TableColumnModelListener;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableColumn;
import net.codecadenza.runtime.richclient.format.FormatDTO;
import net.codecadenza.runtime.richclient.format.FormatPreferencesManager;
import net.codecadenza.runtime.richclient.swing.util.type.Joiner;
import net.codecadenza.runtime.richclient.swing.util.type.Pair;
import org.apache.poi.ss.SpreadsheetVersion;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.CreationHelper;
import org.apache.poi.ss.usermodel.Font;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.jdesktop.swingx.JXTable;
import org.jdesktop.swingx.table.DefaultTableColumnModelExt;
import org.jdesktop.swingx.table.TableColumnExt;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Basic implementation of a visual table component
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the data table
 */
public class JDataTable<T> extends JXTable {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static final long serialVersionUID = 8012253078910843854L;
	private static final String DEFAULT_SHEET_NAME = "Sheet 1";

	protected final transient SortKeyAccessor<T> sortKeyAccessor;
	private transient AbstractDataTableRenderer<T> renderer;
	private final ArrayList<TableDoubleClickListener<T>> clickEventConsumers = new ArrayList<>();
	private final ArrayList<DeleteKeyPressedListener<T>> keyDeleteConsumers = new ArrayList<>();
	private final ArrayList<EnterKeyPressedListener<T>> keyEnterConsumers = new ArrayList<>();
	private final ArrayList<RefreshKeyPressedListener> keyRefreshConsumers = new ArrayList<>();
	protected HashMap<Integer, Integer> originalIndexMap;
	protected boolean initState;

	/**
	 * Constructor
	 * @param renderer
	 * @param sortKeyAccessor
	 */
	public JDataTable(AbstractDataTableRenderer<T> renderer, SortKeyAccessor<T> sortKeyAccessor) {
		super(new TableModel());

		this.sortKeyAccessor = sortKeyAccessor;
		this.renderer = renderer;
		this.renderer.setTable(this);

		setDefaultRenderer(Object.class, this.renderer);
		setEditable(false);
		enableMultiSelectionMode(false);

		addKeyListener(new KeyAdapter() {
			/*
			 * (non-Javadoc) table
			 * @see java.awt.event.KeyAdapter#keyPressed(java.awt.event.KeyEvent)
			 */
			@Override
			public void keyPressed(KeyEvent e) {
				if (e.isConsumed())
					return;

				if (e.getKeyCode() == KeyEvent.VK_F5) {
					keyRefreshConsumers.forEach(RefreshKeyPressedListener::onRefreshKeyPressed);

					e.consume();
				}

				if (e.getKeyCode() == KeyEvent.VK_ENTER) {
					keyEnterConsumers.forEach(listener -> listener.onEnterKeyPressed(getSelectedElement()));

					e.consume();
				}

				if (e.getKeyCode() == KeyEvent.VK_DELETE) {
					keyDeleteConsumers.forEach(listener -> listener.onDeleteKeyPressed(getSelectedElement()));

					e.consume();
				}

				if (e.isControlDown() && e.getKeyCode() == KeyEvent.VK_C) {
					final Pair<Integer, Integer> cell = getSelectedCell();

					if (cell == null)
						return;

					final Clipboard c = Toolkit.getDefaultToolkit().getSystemClipboard();
					final String cellValue = getTextValue(cell);
					final var data = new StringSelection(cellValue);

					c.setContents(data, data);
					e.consume();
				}
			}
		});

		// Add a listener in order get notified when columns are added, deleted or moved!
		getColumnModelAsDefault().addColumnModelListener(new TableColumnModelListener() {
			/*
			 * (non-Javadoc)
			 * @see javax.swing.event.TableColumnModelListener#columnSelectionChanged(javax.swing.event.ListSelectionEvent)
			 */
			@Override
			public void columnSelectionChanged(ListSelectionEvent e) {
				// No implementation required!
			}

			/*
			 * (non-Javadoc)
			 * @see javax.swing.event.TableColumnModelListener#columnRemoved(javax.swing.event.TableColumnModelEvent)
			 */
			@Override
			public void columnRemoved(TableColumnModelEvent e) {
				// No implementation required!
			}

			/*
			 * (non-Javadoc)
			 * @see javax.swing.event.TableColumnModelListener#columnMoved(javax.swing.event.TableColumnModelEvent)
			 */
			@Override
			public void columnMoved(TableColumnModelEvent e) {
				// We shoudn't consume this event if the table is rebuilded completely!
				if (!initState)
					onColumnMoved(e);
			}

			/*
			 * (non-Javadoc)
			 * @see javax.swing.event.TableColumnModelListener#columnMarginChanged(javax.swing.event.ChangeEvent)
			 */
			@Override
			public void columnMarginChanged(ChangeEvent e) {
				// No implementation required!
			}

			/*
			 * (non-Javadoc)
			 * @see javax.swing.event.TableColumnModelListener#columnAdded(javax.swing.event.TableColumnModelEvent)
			 */
			@Override
			public void columnAdded(TableColumnModelEvent e) {
				if (!initState)
					rebuildIndexMap();
			}
		});

		// Add a mouse listener
		addMouseListener(new MouseAdapter() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.MouseAdapter#mouseClicked(java.awt.event.MouseEvent)
			 */
			@Override
			@SuppressWarnings("unchecked")
			public void mouseClicked(MouseEvent e) {
				if (!e.isConsumed() && e.getClickCount() == 2) {
					final Point p = e.getPoint();
					final int r = JDataTable.this.rowAtPoint(p);
					final int c = JDataTable.this.columnAtPoint(p);

					if (r >= 0 && c >= 0) {
						e.consume();
						final T value = (T) JDataTable.this.getValueAt(r, c);

						clickEventConsumers.forEach(listener -> listener.onDblClick(value));
					}
				}
			}
		});

	}

	/**
	 * Interface that defines the sort key access
	 * @param <T> the type of the sort key access
	 */
	public interface SortKeyAccessor<T> {
		Comparable<?> getKey(int modelColumn, T value);
	}

	/**
	 * @param e
	 */
	public void onColumnMoved(TableColumnModelEvent e) {
		if (e.getFromIndex() != e.getToIndex())
			rebuildIndexMap();
	}

	/**
	 * Interface for double-click events
	 * @param <T> the type of the listener
	 */
	public interface TableDoubleClickListener<T> {
		void onDblClick(T selectedElement);
	}

	/**
	 * Register a listener for double-click events
	 * @param listener
	 */
	public void registerDoubleClickListener(TableDoubleClickListener<T> listener) {
		clickEventConsumers.add(listener);
	}

	/**
	 * Interface for delete key-pressed events
	 * @param <T> the type of the listener
	 */
	public interface DeleteKeyPressedListener<T> {
		void onDeleteKeyPressed(T selectedElement);
	}

	/**
	 * Register a listener for delete key-pressed events
	 * @param listener
	 */
	public void registerDeleteKeyPressedListener(DeleteKeyPressedListener<T> listener) {
		keyDeleteConsumers.add(listener);
	}

	/**
	 * Interface for enter key-pressed events
	 * @param <T> the type of the listener
	 */
	public interface EnterKeyPressedListener<T> {
		void onEnterKeyPressed(T selectedElement);
	}

	/**
	 * Register a listener for enter key-pressed events
	 * @param listener
	 */
	public void registerEnterKeyPressedListener(EnterKeyPressedListener<T> listener) {
		keyEnterConsumers.add(listener);
	}

	/**
	 * Interface for refresh key-pressed events
	 */
	public interface RefreshKeyPressedListener {
		void onRefreshKeyPressed();
	}

	/**
	 * Register a listener for refresh key-pressed events
	 * @param listener
	 */
	public void registerRefreshKeyPressedListener(RefreshKeyPressedListener listener) {
		keyRefreshConsumers.add(listener);
	}

	/**
	 * Define if a user may select multiple rows or not!
	 * @param mode
	 */
	public void enableMultiSelectionMode(boolean mode) {
		if (mode)
			setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		else
			setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
	}

	/**
	 * @return true if only a single row can be selected at a time
	 */
	public boolean isSingleSelection() {
		return getSelectionMode() == ListSelectionModel.SINGLE_SELECTION;
	}

	/**
	 * @param component
	 */
	public void addTo(JComponent component) {
		component.add(new JScrollPane(this), BorderLayout.CENTER);
	}

	/**
	 * @return the selected element
	 */
	@SuppressWarnings("unchecked")
	public T getSelectedElement() {
		if (getSelectedRow() == -1)
			return null;

		final int i = convertRowIndexToModel(getSelectedRow());
		return i >= 0 ? (T) getTableModel().getValueAt(i, 0) : null;
	}

	/**
	 * Set the data
	 * @param data
	 */
	@SuppressWarnings("unchecked")
	public void setData(Collection<T> data) {
		final TableModel model = getTableModel();
		model.setData((Collection<Object>) data);
	}

	/**
	 * @return the table model
	 */
	private TableModel getTableModel() {
		return (TableModel) getModel();
	}

	/**
	 * Add columns to the table model
	 * @param columns
	 */
	protected void setColumns(String... columns) {
		getTableModel().setColumnIdentifiers(columns);
	}

	/**
	 * @return the row sorter
	 */
	private DefaultRowSorter<?, ?> getSorterAsDefault() {
		return (DefaultRowSorter<?, ?>) getRowSorter();
	}

	/**
	 * Set the sorter for a specific column
	 * @param columnIndex
	 * @param ordering
	 */
	public void setColumnSorter(int columnIndex, Comparator<?> ordering) {
		getSorterAsDefault().setComparator(columnIndex, ordering);
	}

	/**
	 * Implementation of the table model
	 */
	public static class TableModel extends AbstractTableModel {
		private static final long serialVersionUID = -1105941626008082083L;

		private String[] columnHeadings = {};
		private transient List<Object> rows = Collections.emptyList();

		/*
		 * (non-Javadoc)
		 * @see javax.swing.table.TableModel#getColumnCount()
		 */
		@Override
		public int getColumnCount() {
			return columnHeadings.length;
		}

		/**
		 * @return the column titles
		 */
		public List<String> getColumnHeadings() {
			return List.of(columnHeadings);
		}

		/*
		 * (non-Javadoc)
		 * @see javax.swing.table.AbstractTableModel#getColumnName(int)
		 */
		@Override
		public String getColumnName(int column) {
			return columnHeadings[column];
		}

		/**
		 * @param columns
		 */
		public void setColumnIdentifiers(String[] columns) {
			this.columnHeadings = columns;
			fireTableStructureChanged();
		}

		/*
		 * (non-Javadoc)
		 * @see javax.swing.table.TableModel#getRowCount()
		 */
		@Override
		public int getRowCount() {
			return rows.size();
		}

		/*
		 * (non-Javadoc)
		 * @see javax.swing.table.TableModel#getValueAt(int, int)
		 */
		@Override
		public Object getValueAt(int rowIndex, int columnIndex) {
			return rows.get(rowIndex);
		}

		/**
		 * @param rows
		 */
		public void setData(Collection<Object> rows) {
			this.rows = rows.stream().collect(Collectors.toList());

			fireTableDataChanged();
		}

		/**
		 * @return the rows
		 */
		public List<Object> getRows() {
			return rows;
		}
	}

	/**
	 * @return the column model
	 */
	protected DefaultTableColumnModelExt getColumnModelAsDefault() {
		return (DefaultTableColumnModelExt) getColumnModel();
	}

	/**
	 * @param columnList
	 */
	@SuppressWarnings("unchecked")
	public void initColumns(List<ColumnInfo> columnList) {
		// If the column definition list doesn't exist we assume that the client uses a different approach!
		if (columnList == null || columnList.isEmpty())
			return;

		initState = true;

		// Remove all existing columns
		final List<TableColumn> tableCols = getColumns(true);

		tableCols.forEach(this::removeColumn);

		// Sort the columns based on the index before adding them to the table
		columnList.sort((col1, col2) -> col1.getIndex() - col2.getIndex());

		final var headings = new ArrayList<String>();

		columnList.forEach(col -> headings.add(col.getTitle()));

		final var hs = new String[headings.size()];
		headings.toArray(hs);

		setColumns(hs);

		final DefaultTableColumnModelExt cols = getColumnModelAsDefault();

		int i = 0;

		for (final ColumnInfo colInfo : columnList) {
			final TableColumnExt col = cols.getColumnExt(i);
			col.setTitle(colInfo.getTitle());
			col.setPreferredWidth(colInfo.getWidth());
			col.setResizable(true);

			col.setComparator((o1, o2) -> {
				final var val1 = (Comparable<Object>) sortKeyAccessor.getKey(col.getModelIndex(), (T) o1);
				final var val2 = (Comparable<Object>) sortKeyAccessor.getKey(col.getModelIndex(), (T) o2);

				if (val1 != null && val2 != null)
					return val1.compareTo(val2);
				else if (val1 == null && val2 != null)
					return -1;
				else if (val1 != null)
					return 1;
				else
					return 0;
			});

			i++;
		}

		initState = false;

		rebuildIndexMap();
	}

	/**
	 * Replace all carriage return and line feed characters using a blank
	 * @param input
	 * @return the input string without carriage return and line feed characters
	 */
	protected String replaceCRLFCharacters(String input) {
		return input.replace("\r\n", " ").replace("\r", " ").replace("\n", " ").trim();
	}

	/**
	 * @return a list containing all table columns
	 */
	public List<TableColumnExt> getAllTableColumns() {
		final var list = new ArrayList<TableColumnExt>();

		getColumnModelAsDefault().getColumns(true).forEach(col -> list.add((TableColumnExt) col));

		return list;
	}

	/**
	 * @return a list containing information about all columns
	 */
	public List<ColumnInfo> getColumnInfos() {
		final List<TableColumnExt> cols = getAllTableColumns();
		final var cs = new ColumnInfo[cols.size()];

		cols.forEach(col -> {
			final var colInfo = new ColumnInfo(col.getModelIndex(), col.isVisible(), convertColumnIndexToView(col.getModelIndex()),
					col.getTitle());
			cs[col.getModelIndex()] = colInfo;
		});

		final List<ColumnInfo> list = Arrays.asList(cs);

		// Sort the columns based on the index before adding them to the table
		list.sort((col1, col2) -> col1.getIndex() - col2.getIndex());

		return list;
	}

	/**
	 * Rebuild the index
	 */
	protected void rebuildIndexMap() {
		originalIndexMap = new HashMap<>();

		for (final ColumnInfo col : getColumnInfos())
			if (col.isVisible())
				originalIndexMap.put(col.getVisibleIndex(), col.getIndex());
	}

	/**
	 * @return the content to be exported
	 */
	@SuppressWarnings("unchecked")
	public String getExportContent() {
		final var content = new StringBuilder();
		int columnIndex = 0;
		T element = null;
		final String csvSep = FormatPreferencesManager.getFormatDTO().getCsvSeparator();
		final Joiner joiner = Joiner.on(csvSep);
		final TableModel model = getTableModel();

		final List<String> cols = getColumnTitles();
		joiner.appendTo(content, cols, true);

		final String lineSep = System.lineSeparator();
		content.append(lineSep);

		for (int rowModelIndex = 0; rowModelIndex < model.getRowCount(); rowModelIndex++) {
			final var rowContent = new ArrayList<String>();

			for (final ColumnInfo col : getColumnInfos()) {
				if (!col.isVisible())
					continue;

				element = (T) getTableModel().getValueAt(convertRowIndexToModel(rowModelIndex), 0);

				columnIndex = originalIndexMap.get(col.getVisibleIndex());
				rowContent.add(replaceCRLFCharacters(renderer.getCellExportText(element, columnIndex)));
			}

			joiner.appendTo(content, rowContent, true);
			content.append(lineSep);
		}

		return content.toString();
	}

	/**
	 * @param file
	 * @throws Exception if either the provided file doesn't exist, or if the content could not be written to this file
	 */
	@SuppressWarnings("unchecked")
	public void writeTableContentToXLSFile(File file) throws Exception {
		FileOutputStream fileOutputStream = null;
		final FormatDTO userFormat = FormatPreferencesManager.getFormatDTO();
		final var dateColumnIndexSet = new HashSet<Integer>();
		int colCounter = 0;
		int rowCounter = 0;
		int columnIndex = 0;

		try (XSSFWorkbook workbook = new XSSFWorkbook()) {
			// Create a new sheet
			final XSSFSheet sheet = workbook.createSheet(DEFAULT_SHEET_NAME);
			final CreationHelper createHelper = workbook.getCreationHelper();

			// Define different cell styles
			final CellStyle cellStyleDateTime = workbook.createCellStyle();
			cellStyleDateTime.setDataFormat(createHelper.createDataFormat().getFormat(userFormat.getDateTimeFormat()));

			final CellStyle cellStyleDate = workbook.createCellStyle();
			cellStyleDate.setDataFormat(createHelper.createDataFormat().getFormat(userFormat.getDateFormat()));

			final CellStyle cellStyleDouble = workbook.createCellStyle();
			cellStyleDouble.setDataFormat(createHelper.createDataFormat().getFormat(userFormat.getDecimalFormat()));

			final Font headerFont = workbook.createFont();
			headerFont.setFontHeightInPoints((short) 11);
			headerFont.setBold(true);

			final CellStyle headerCellStyle = workbook.createCellStyle();
			headerCellStyle.setFont(headerFont);

			// Create the header row
			final Row headerRow = sheet.createRow(rowCounter++);
			final List<String> cols = getColumnTitles();

			for (final String colLabel : cols) {
				final Cell cell = headerRow.createCell(colCounter++);
				cell.setCellValue(colLabel);
				cell.setCellStyle(headerCellStyle);
			}

			for (int rowModelIndex = 0; rowModelIndex < getTableModel().getRowCount(); rowModelIndex++) {
				if ((rowCounter + 1) == SpreadsheetVersion.EXCEL2007.getMaxRows())
					break;

				final Row row = sheet.createRow(rowCounter++);

				for (final ColumnInfo col : getColumnInfos()) {
					if (!col.isVisible())
						continue;

					final T element = (T) getTableModel().getValueAt(convertRowIndexToModel(rowModelIndex), 0);

					columnIndex = originalIndexMap.get(col.getVisibleIndex());
					final Comparable<?> value = renderer.getCellValue(element, columnIndex);

					final Cell cell = row.createCell(col.getVisibleIndex());

					if (value == null)
						continue;

					if (value instanceof final String stringValue)
						cell.setCellValue(stringValue);
					else if (value instanceof final Integer integerValue)
						cell.setCellValue(integerValue);
					else if (value instanceof final Long longValue)
						cell.setCellValue(longValue);
					else if (value instanceof final Boolean booleanValue)
						cell.setCellValue(booleanValue);
					else if (value instanceof final UUID uuidValue)
						cell.setCellValue(uuidValue.toString());
					else if (value instanceof final Double doubleValue) {
						cell.setCellValue(doubleValue);
						cell.setCellStyle(cellStyleDouble);
					}
					else if (value instanceof final BigDecimal bigDecimalValue) {
						cell.setCellValue(bigDecimalValue.doubleValue());
						cell.setCellStyle(cellStyleDouble);
					}
					else if (value instanceof final Float floatValue) {
						cell.setCellValue(floatValue);
						cell.setCellStyle(cellStyleDouble);
					}
					else if (value instanceof final Date dateValue) {
						cell.setCellValue(dateValue);
						cell.setCellStyle(cellStyleDateTime);

						dateColumnIndexSet.add(col.getVisibleIndex());
					}
					else if (value instanceof final Calendar calendarValue) {
						cell.setCellValue(calendarValue);
						cell.setCellStyle(cellStyleDateTime);

						dateColumnIndexSet.add(col.getVisibleIndex());
					}
					else if (value instanceof final LocalDate localDateValue) {
						cell.setCellValue(Date.from(localDateValue.atStartOfDay().atZone(ZoneId.systemDefault()).toInstant()));
						cell.setCellStyle(cellStyleDate);

						dateColumnIndexSet.add(col.getVisibleIndex());
					}
					else if (value instanceof final LocalDateTime localDateTimeValue) {
						cell.setCellValue(Date.from(localDateTimeValue.atZone(ZoneId.systemDefault()).toInstant()));
						cell.setCellStyle(cellStyleDateTime);

						dateColumnIndexSet.add(col.getVisibleIndex());
					}
				}
			}

			dateColumnIndexSet.forEach(sheet::autoSizeColumn);

			fileOutputStream = new FileOutputStream(file);
			workbook.write(fileOutputStream);
		}
		finally {
			try {
				if (fileOutputStream != null)
					fileOutputStream.close();
			}
			catch (final IOException ex) {
				logger.warn("Could not close file output stream", ex);
			}
		}
	}

	/**
	 * @return the labels of all visible columns
	 */
	public List<String> getColumnTitles() {
		final var hs = new ArrayList<String>();

		for (final ColumnInfo col : getColumnInfos())
			if (col.isVisible())
				hs.add(col.getTitle());

		return hs;
	}

	/**
	 * @param value
	 * @param columnIndex
	 * @return the text value of a cell
	 */
	private String getTextValueOf(Object value, int columnIndex) {
		return renderer.getTableCellText(value, originalIndexMap.get(columnIndex));
	}

	/**
	 * @return the column index map
	 */
	public Map<Integer, Integer> getOriginalIndexMap() {
		return originalIndexMap;
	}

	/**
	 * @param row
	 * @param columnIndex
	 * @return the text value of a cell
	 */
	public String getTextValue(int row, int columnIndex) {
		final Object obj = getTableModel().getValueAt(convertRowIndexToModel(row), originalIndexMap.get(columnIndex));

		return getTextValueOf(obj, columnIndex);
	}

	/**
	 * @param cellPos
	 * @return the text value of a cell
	 */
	public String getTextValue(Pair<Integer, Integer> cellPos) {
		return getTextValue(cellPos.first(), cellPos.second());
	}

	/**
	 * @return the selected row text
	 */
	@SuppressWarnings("unchecked")
	public List<String> getSelectedRowText() {
		final var rowContent = new ArrayList<String>();
		int columnIndex = 0;
		final int row = getSelectedRow();

		if (row == -1)
			return Collections.emptyList();

		final T element = (T) getTableModel().getValueAt(convertRowIndexToModel(row), 0);

		for (final ColumnInfo col : getColumnInfos()) {
			if (!col.isVisible())
				continue;

			columnIndex = originalIndexMap.get(col.getVisibleIndex());
			final String cellExportText = replaceCRLFCharacters(renderer.getCellExportText(element, columnIndex));

			rowContent.add(cellExportText);
		}

		return rowContent;
	}

	/**
	 * @return the selected row text
	 */
	@SuppressWarnings("unchecked")
	public List<String> getSelectedColumnText() {
		final int col = getSelectedColumn();

		if (col == -1)
			return Collections.emptyList();

		final var columnContent = new ArrayList<String>();
		final int columnIndex = originalIndexMap.get(col);

		for (int i = 0; i < getRowCount(); i++) {
			final T element = (T) getTableModel().getValueAt(convertRowIndexToModel(i), 0);
			columnContent.add(replaceCRLFCharacters(renderer.getCellExportText(element, columnIndex)));
		}

		return columnContent;
	}

	/**
	 * @return a list containing all selected elements
	 */
	@SuppressWarnings("unchecked")
	public List<T> getSelectedElements() {
		final var sel = new ArrayList<T>();

		for (final int i : getSelectedRows())
			sel.add((T) getTableModel().getValueAt(convertRowIndexToModel(i), 0));

		return sel;
	}

	/**
	 * Remove the element from the model
	 * @param element
	 */
	public void removeElementFromModel(T element) {
		getTableModel().getRows().remove(element);
		getTableModel().fireTableDataChanged();
	}

	/**
	 * @return the selected cell
	 */
	public Pair<Integer, Integer> getSelectedCell() {
		final int c = getSelectedColumn();

		if (c == -1)
			return null;

		final int r = getSelectedRow();

		if (r == -1)
			return null;

		return Pair.pair(r, c);
	}

	/**
	 * @return the table renderer of this table
	 */
	public AbstractDataTableRenderer<T> getRenderer() {
		return renderer;
	}

}
