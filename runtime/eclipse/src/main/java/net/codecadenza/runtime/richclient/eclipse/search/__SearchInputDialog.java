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
package net.codecadenza.runtime.richclient.eclipse.search;

import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.CMD_CANCEL;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.SEARCH_INPUT_DIALOG_AVAILABLE_FIELDS;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.SEARCH_INPUT_DIALOG_BETWEEN_TOOLTIP;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.SEARCH_INPUT_DIALOG_CB_EXACT_FILTER_MATCH;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.SEARCH_INPUT_DIALOG_CHK_CASE;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.SEARCH_INPUT_DIALOG_CHK_COUNT;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.SEARCH_INPUT_DIALOG_CMD_CLEAR;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.SEARCH_INPUT_DIALOG_CMD_COUNT;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.SEARCH_INPUT_DIALOG_CMD_INV_SORT;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.SEARCH_INPUT_DIALOG_CMD_MOVE_DOWN;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.SEARCH_INPUT_DIALOG_CMD_MOVE_UP;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.SEARCH_INPUT_DIALOG_CMD_REMOVE_SORT_ITEM;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.SEARCH_INPUT_DIALOG_CMD_REMOVE_SORT_ITEMS;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.SEARCH_INPUT_DIALOG_CMD_SEARCH;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.SEARCH_INPUT_DIALOG_LBL_SORT;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.SEARCH_INPUT_DIALOG_MAX_FETCH_SIZE;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.SEARCH_INPUT_DIALOG_MSG_COUNT_ERROR;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.SEARCH_INPUT_DIALOG_MSG_COUNT_RES;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.SEARCH_INPUT_DIALOG_MSG_ERR_CRITERION_EXP;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.SEARCH_INPUT_DIALOG_MSG_ERR_DATE_FORMAT;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.SEARCH_INPUT_DIALOG_MSG_ERR_LOV;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.SEARCH_INPUT_DIALOG_MSG_ERR_MIN_FIELD_COUNT;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.SEARCH_INPUT_DIALOG_MSG_ERR_MISSING_BETWEEN;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.SEARCH_INPUT_DIALOG_MSG_ERR_MISSING_IN;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.SEARCH_INPUT_DIALOG_MSG_ERR_NO_NUMBER;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.SEARCH_INPUT_DIALOG_MSG_ERR_NO_UUID;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.SEARCH_INPUT_DIALOG_SEL_FIELDS;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.SEARCH_INPUT_DIALOG_TAB_ADV_SETTINGS;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.SEARCH_INPUT_DIALOG_TAB_FILTER;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.SEARCH_INPUT_DIALOG_TITLE;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.SEARCH_INPUT_DIALOG_TITLE_MSG;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.SEARCH_INPUT_DIALOG_TOOLTIP_UP_DOWN;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.SEARCH_INPUT_DIALOG_VISUAL_FIELDS;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.getTranslation;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.getTranslationForFieldLabel;
import static net.codecadenza.runtime.search.SearchService.OPERATOR_BETWEEN;
import static net.codecadenza.runtime.search.SearchService.OPERATOR_EQUAL;
import static net.codecadenza.runtime.search.SearchService.OPERATOR_IN;
import static net.codecadenza.runtime.search.SearchService.OPERATOR_IS_NOT_NULL;
import static net.codecadenza.runtime.search.SearchService.OPERATOR_IS_NULL;
import static net.codecadenza.runtime.search.SearchService.OPERATOR_LIKE;
import static net.codecadenza.runtime.search.SearchService.OPERATOR_NOT_IN;
import static net.codecadenza.runtime.search.SearchService.OPERATOR_NOT_LIKE;
import static net.codecadenza.runtime.search.SearchService.TOKEN_DELIMITER_BETWEEN;
import static net.codecadenza.runtime.search.SearchService.TOKEN_DELIMITER_IN;

import java.lang.invoke.MethodHandles;
import java.lang.reflect.Constructor;
import java.text.DecimalFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.UUID;
import java.util.regex.Pattern;
import net.codecadenza.runtime.richclient.eclipse.image.ImageCache;
import net.codecadenza.runtime.richclient.format.FormatDTO;
import net.codecadenza.runtime.search.dto.SearchDTO;
import net.codecadenza.runtime.search.dto.SearchFieldDTO;
import net.codecadenza.runtime.search.dto.SearchFieldDataTypeEnum;
import net.codecadenza.runtime.search.dto.SearchFieldTypeEnum;
import net.codecadenza.runtime.search.dto.SearchOperatorDTO;
import net.codecadenza.runtime.search.dto.SortDirectionEnum;
import net.codecadenza.runtime.search.util.SearchOperatorHelper;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.DragSource;
import org.eclipse.swt.dnd.DragSourceEvent;
import org.eclipse.swt.dnd.DragSourceListener;
import org.eclipse.swt.dnd.DropTarget;
import org.eclipse.swt.dnd.DropTargetAdapter;
import org.eclipse.swt.dnd.DropTargetEvent;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.Text;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Dialog for entering filter criteria, sort orders, fetch size and the fields to be displayed in a search result view
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class __SearchInputDialog extends TitleAreaDialog {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	public static final int MIN_VISUAL_FIELD_COUNT = 2;
	protected static final int DIALOG_MIN_HEIGHT = 585;
	protected static final int DIALOG_HEIGHT_OFFSET = 160;
	protected static final int ROW_HEIGHT_FACTOR = 25;
	protected static final String DIALOG_TITLE = getTranslation(SEARCH_INPUT_DIALOG_TITLE);
	private static final Pattern TOKEN_DELIMITER_IN_PATTERN = Pattern.compile(TOKEN_DELIMITER_IN);
	private static final Pattern TOKEN_DELIMITER_BETWEEN_PATTERN = Pattern.compile(TOKEN_DELIMITER_BETWEEN);

	protected Combo cboFetchSize;
	protected Countable countable;
	protected SearchDTO searchObj;
	protected Button chkCase;
	protected Composite panDialogArea;
	protected int rowCount;
	protected String[] sortOrders = new String[3];
	protected Button chkCount;
	protected Shell thisShell;
	protected Table tableAllFields;
	protected Table tableSelFields;
	protected TableViewer tableViewerAllFields;
	protected TableViewer tableViewerSelFields;
	protected Button chkExact;
	protected TableViewer tableViewerSorting;
	protected Table tableSorting;
	protected HashMap<String, Combo> operatorMap = new HashMap<>();
	protected HashMap<String, Control> criterion1Map = new HashMap<>();
	protected HashMap<String, Text> criterion2Map = new HashMap<>();
	protected HashMap<String, Label> labelCrit2Map = new HashMap<>();
	protected FormatDTO userFormat;
	protected DecimalFormat decimalFormat;
	protected SimpleDateFormat dateTimeFormat;
	protected SimpleDateFormat dateFormat;

	/**
	 * Create the search input dialog
	 * @param parentShell
	 * @param searchObj
	 * @param countable
	 */
	protected __SearchInputDialog(Shell parentShell, SearchDTO searchObj, Countable countable) {
		super(parentShell);

		this.searchObj = searchObj;
		this.rowCount = searchObj.getSearchFields().size();
		this.countable = countable;
		this.thisShell = parentShell;
		this.userFormat = getFormatPreferences();
		this.decimalFormat = new DecimalFormat(userFormat.getDecimalFormat());
		this.dateTimeFormat = new SimpleDateFormat(userFormat.getDateTimeFormat());
		this.dateFormat = new SimpleDateFormat(userFormat.getDateFormat());
		this.setShellStyle(super.getShellStyle() | SWT.RESIZE);
	}

	/**
	 * @return the search input object
	 */
	public SearchDTO getSearchInput() {
		return searchObj;
	}

	/**
	 * An implementation must define how a proposal adapter should be added to a given text field
	 * @param textField
	 * @param lovCommand
	 */
	protected abstract void addProposalAdapter(Text textField, String lovCommand);

	/**
	 * @return the format preferences
	 */
	protected abstract FormatDTO getFormatPreferences();

	/**
	 * Comparator that compares two search fields by using the column order
	 */
	class ColOrderComparator extends ViewerComparator {
		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ViewerComparator#compare(org.eclipse.jface.viewers.Viewer, java.lang.Object,
		 * java.lang.Object)
		 */
		@Override
		public int compare(Viewer viewer, Object e1, Object e2) {
			final var item1 = (SearchFieldDTO) e1;
			final var item2 = (SearchFieldDTO) e2;

			return item1.getColOrder() - item2.getColOrder();
		}
	}

	/**
	 * Comparator that compares two search fields by using the label
	 */
	class ColLabelComparator extends ViewerComparator {
		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ViewerComparator#compare(org.eclipse.jface.viewers.Viewer, java.lang.Object,
		 * java.lang.Object)
		 */
		@Override
		public int compare(Viewer viewer, Object e1, Object e2) {
			final var item1 = (SearchFieldDTO) e1;
			final var item2 = (SearchFieldDTO) e2;

			return item1.getColLabel().compareTo(item2.getColLabel());
		}
	}

	/**
	 * Label provider for fields
	 */
	class FieldLabelProvider extends LabelProvider implements ITableLabelProvider {
		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnImage(java.lang.Object, int)
		 */
		@Override
		public Image getColumnImage(Object element, int colIndex) {
			if (colIndex == 0)
				return ImageCache.getImage(ImageCache.IMG_ITEM);

			return null;
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnText(java.lang.Object, int)
		 */
		@Override
		public String getColumnText(Object element, int colIndex) {
			final var item = (SearchFieldDTO) element;

			if (colIndex == 0)
				return item.getColLabel();

			return null;
		}
	}

	/**
	 * Content provider for fields
	 */
	class AllFieldsContentProvider implements IStructuredContentProvider {
		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
		 */
		@Override
		public Object[] getElements(Object inputElement) {
			return searchObj.getSearchFields().stream().filter(field -> !field.isVisible()).toArray(SearchFieldDTO[]::new);
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.IContentProvider#dispose()
		 */
		@Override
		public void dispose() {
			// No implementation required!
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object,
		 * java.lang.Object)
		 */
		@Override
		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
			// No implementation required!
		}
	}

	/**
	 * Content provider for columns
	 */
	class SelectedFieldsContentProvider implements IStructuredContentProvider {
		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
		 */
		@Override
		public Object[] getElements(Object inputElement) {
			return searchObj.getSearchFields().stream().filter(SearchFieldDTO::isVisible).toArray(SearchFieldDTO[]::new);
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.IContentProvider#dispose()
		 */
		@Override
		public void dispose() {
			// No implementation required!
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object,
		 * java.lang.Object)
		 */
		@Override
		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
			// No implementation required!
		}
	}

	/**
	 * Content provider
	 */
	class ContentProvider implements IStructuredContentProvider {
		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
		 */
		@Override
		public Object[] getElements(Object inputElement) {
			return searchObj.getSearchFields().stream().toArray(SearchFieldDTO[]::new);
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.IContentProvider#dispose()
		 */
		@Override
		public void dispose() {
			// No implementation required!
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object,
		 * java.lang.Object)
		 */
		@Override
		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
			// No implementation required!
		}
	}

	/**
	 * Content provider for sorting
	 */
	class SortContentProvider implements IStructuredContentProvider {
		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
		 */
		@Override
		public Object[] getElements(Object inputElement) {
			return searchObj.getSearchFields().stream().filter(field -> field.isVisible() && field.getSortIndex() > 0)
					.toArray(SearchFieldDTO[]::new);
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.IContentProvider#dispose()
		 */
		@Override
		public void dispose() {
			// No implementation required!
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object,
		 * java.lang.Object)
		 */
		@Override
		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
			// No implementation required!
		}
	}

	/**
	 * Label provider for sorting
	 */
	class SortLabelProvider extends LabelProvider implements ITableLabelProvider {
		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnImage(java.lang.Object, int)
		 */
		@Override
		public Image getColumnImage(Object element, int colIndex) {
			if (colIndex == 0) {
				final var item = (SearchFieldDTO) element;

				if (item.getSortOrder() == SortDirectionEnum.ASC)
					return ImageCache.getImage(ImageCache.IMG_SORT_ASC);

				return ImageCache.getImage(ImageCache.IMG_SORT_DESC);
			}

			return null;
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnText(java.lang.Object, int)
		 */
		@Override
		public String getColumnText(Object element, int colIndex) {
			final var item = (SearchFieldDTO) element;

			if (colIndex == 0)
				return item.getColLabel() + " (" + item.getSortOrder().name() + ")";

			return null;
		}
	}

	/**
	 * Comparator that compares two search fields by using the sort index
	 */
	class SortComparator extends ViewerComparator {
		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ViewerComparator#compare(org.eclipse.jface.viewers.Viewer, java.lang.Object,
		 * java.lang.Object)
		 */
		@Override
		public int compare(Viewer viewer, Object e1, Object e2) {
			final var item1 = (SearchFieldDTO) e1;
			final var item2 = (SearchFieldDTO) e2;

			return item1.getSortIndex() - item2.getSortIndex();
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.TitleAreaDialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		try {
			// Define a key adapter to test if the user presses the 'ENTER' key to start a search operation
			final var keyAdapter = new KeyAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.KeyAdapter#keyReleased(org.eclipse.swt.events.KeyEvent)
				 */
				@Override
				public void keyReleased(KeyEvent e) {
					if (e.keyCode == SWT.F8)
						buttonPressed(IDialogConstants.OK_ID);
				}
			};

			panDialogArea = (Composite) super.createDialogArea(parent);

			final var panDialog = new Composite(panDialogArea, SWT.NONE);
			panDialog.setLayout(new GridLayout(4, false));
			panDialog.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

			final var tabFolder = new TabFolder(panDialog, SWT.NONE);
			tabFolder.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 4, 1));

			final var tabItemFilter = new TabItem(tabFolder, SWT.NONE);
			tabItemFilter.setText(getTranslation(SEARCH_INPUT_DIALOG_TAB_FILTER));

			final var scrolledComposite = new ScrolledComposite(tabFolder, SWT.BORDER | SWT.V_SCROLL);
			scrolledComposite.setExpandHorizontal(true);
			scrolledComposite.setAlwaysShowScrollBars(true);

			final var gridLayout = new GridLayout(6, false);
			gridLayout.marginHeight = 2;
			gridLayout.marginWidth = 2;
			gridLayout.horizontalSpacing = 2;
			gridLayout.verticalSpacing = 2;

			// Composite for filter criteria
			final var panFilter = new Composite(scrolledComposite, SWT.NONE);
			panFilter.setLayout(gridLayout);

			tabItemFilter.setControl(scrolledComposite);

			// Grid data for operator fields
			final var gdOperators = new GridData(SWT.LEFT, SWT.CENTER, false, false);
			gdOperators.widthHint = 80;

			// Grid data for filter fields with a combobox (boolean and enumeration)
			final var gdComboCriterion = new GridData(SWT.LEFT, SWT.CENTER, false, false);
			gdComboCriterion.widthHint = 150;

			// Grid data for images that are used for push buttons
			final var gdPushButton = new GridData(SWT.LEFT, SWT.CENTER, false, false);
			gdPushButton.widthHint = 25;
			gdPushButton.heightHint = 16;

			// Grid data for list-of-values fields
			final var gdLOV = new GridData(SWT.FILL, SWT.CENTER, true, false, 3, 1);

			// Grid data for simple text fields
			final var gdString = new GridData(SWT.FILL, SWT.CENTER, true, false, 3, 1);

			// Add all search input fields
			for (final SearchFieldDTO s : searchObj.getSearchFields()) {
				final String key = s.getColLabel();

				if (s.getType() == SearchFieldTypeEnum.NOT_SEARCHABLE)
					continue;

				final var fieldLabel = new Label(panFilter, SWT.NONE);
				fieldLabel.setText(s.getColLabel());

				// Create all controls. If they are visible is decided later
				final var c = new Combo(panFilter, SWT.READ_ONLY);
				c.setLayoutData(gdOperators);

				operatorMap.put(key, c);

				// Boolean or enumeration fields should use a combobox
				Text txtCrit1 = null;
				Combo cbCrit1 = null;

				if (s.getDataType() == SearchFieldDataTypeEnum.BOOLEAN || s.getDataType() == SearchFieldDataTypeEnum.ENUM) {
					cbCrit1 = new Combo(panFilter, SWT.READ_ONLY);
					cbCrit1.setLayoutData(gdComboCriterion);
					cbCrit1.addKeyListener(keyAdapter);

					criterion1Map.put(key, cbCrit1);
				}
				else if (s.getLovCommand() != null) {
					txtCrit1 = new Text(panFilter, SWT.BORDER);
					txtCrit1.setLayoutData(gdLOV);
					txtCrit1.addKeyListener(keyAdapter);

					criterion1Map.put(key, txtCrit1);

					// Add a proposal adapter
					if (s.getDataType() == SearchFieldDataTypeEnum.STRING)
						addProposalAdapter(txtCrit1, s.getLovCommand());
				}
				else {
					txtCrit1 = new Text(panFilter, SWT.BORDER);

					if (s.getDataType() == SearchFieldDataTypeEnum.STRING || s.getDataType() == SearchFieldDataTypeEnum.CHAR
							|| s.getDataType() == SearchFieldDataTypeEnum.UUID_BINARY || s.getDataType() == SearchFieldDataTypeEnum.UUID_STRING)
						txtCrit1.setLayoutData(gdString);
					else
						txtCrit1.setLayoutData(gdComboCriterion);

					txtCrit1.addKeyListener(keyAdapter);

					criterion1Map.put(key, txtCrit1);
				}

				if (txtCrit1 != null)
					addFeaturesTextInputField(txtCrit1, c, s);

				final var lblCrit1 = new Label(panFilter, SWT.NONE);
				lblCrit1.setLayoutData(gdPushButton);

				if (s.getListOfValues() != null)
					lblCrit1.setImage(ImageCache.getImage(ImageCache.IMG_LOV));
				else if (s.hasTemporalDataType())
					lblCrit1.setImage(ImageCache.getImage(ImageCache.IMG_CALENDAR));

				// This is a little hack in order to "hide" form fields that are not used at all! The second text entry field and the
				// second image label are not necessary for list-of-values, simple string, character and UUID entry fields!
				Composite panCompParent = null;

				if (s.getLovCommand() != null || s.getDataType() == SearchFieldDataTypeEnum.STRING
						|| s.getDataType() == SearchFieldDataTypeEnum.CHAR || s.getDataType() == SearchFieldDataTypeEnum.UUID_BINARY
						|| s.getDataType() == SearchFieldDataTypeEnum.UUID_STRING) {
					panCompParent = new Composite(scrolledComposite, SWT.NONE);
					panCompParent.setVisible(false);
				}
				else
					panCompParent = panFilter;

				final var txtCrit2 = new Text(panCompParent, SWT.BORDER);
				txtCrit2.setLayoutData(gdComboCriterion);
				txtCrit2.setToolTipText(getTranslation(SEARCH_INPUT_DIALOG_BETWEEN_TOOLTIP));
				txtCrit2.addKeyListener(keyAdapter);

				criterion2Map.put(key, txtCrit2);

				final var lblCrit2 = new Label(panCompParent, SWT.NONE);
				lblCrit2.setLayoutData(gdPushButton);
				lblCrit2.setToolTipText(getTranslation(SEARCH_INPUT_DIALOG_BETWEEN_TOOLTIP));

				labelCrit2Map.put(key, lblCrit2);

				if (s.hasTemporalDataType())
					lblCrit2.setImage(ImageCache.getImage(ImageCache.IMG_CALENDAR));

				// Fill the operator combobox with the supported operators
				final var opList = new ArrayList<String>();
				opList.add("");
				opList.addAll(SearchOperatorHelper.getOperatorsForField(s).stream().map(SearchOperatorDTO::getDescription).toList());

				c.setItems(opList.toArray(new String[opList.size()]));

				// Preselect the operator if the operator is already set in the respective search field
				if (s.getOperator() != null) {
					final String selectedOperator = s.getOperator().getDescription();

					for (int i = 0; i < c.getItemCount(); i++)
						if (c.getItem(i).equals(selectedOperator)) {
							c.select(i);
							break;
						}
				}
				else
					c.select(0);

				// Selection listener that reacts to the selected operator
				c.addSelectionListener(new SelectionAdapter() {
					/*
					 * (non-Javadoc)
					 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
					 */
					@Override
					public void widgetSelected(SelectionEvent e) {
						// Determine the selected operator
						final String selectedOperator = c.getItem(c.getSelectionIndex());
						final SearchOperatorDTO operator = SearchOperatorHelper.getAllOperators().stream()
								.filter(sod -> sod.getDescription().equals(selectedOperator)).findFirst().orElse(null);

						if (operator == null) {
							criterion1Map.get(key).setVisible(true);
							criterion2Map.get(key).setVisible(false);
							criterion2Map.get(key).setText("");
							lblCrit1.setVisible(s.hasTemporalDataType() || s.getListOfValues() != null);
							lblCrit2.setVisible(false);
						}
						else if (operator.isExpectsArgument()) {
							criterion1Map.get(key).setVisible(true);
							criterion2Map.get(key).setVisible(false);
							lblCrit1.setVisible(false);
							lblCrit2.setVisible(false);

							if (s.hasTemporalDataType() || s.getListOfValues() != null)
								lblCrit1.setVisible(true);

							if (operator.getDescription().equals(OPERATOR_BETWEEN)) {
								criterion2Map.get(key).setVisible(true);

								if (s.hasTemporalDataType())
									lblCrit2.setVisible(true);
							}
							else
								criterion2Map.get(key).setText("");
						}
						else {
							criterion1Map.get(key).setVisible(false);

							if (criterion1Map.get(key).getClass().isAssignableFrom(Text.class))
								((Text) criterion1Map.get(key)).setText("");
							else
								((Combo) criterion1Map.get(key)).setText("");

							criterion2Map.get(key).setVisible(false);
							criterion2Map.get(key).setText("");
							lblCrit1.setVisible(false);
							lblCrit2.setVisible(false);
						}
					}
				});

				// Modify criteria field 1
				final String criteria = s.getFilterCriteria();

				if (s.getDataType() == SearchFieldDataTypeEnum.BOOLEAN) {
					((Combo) criterion1Map.get(key)).setItems("", Boolean.TRUE.toString(), Boolean.FALSE.toString());

					if (criteria != null && !criteria.isEmpty()) {
						if (Boolean.TRUE.toString().equals(criteria))
							((Combo) criterion1Map.get(key)).select(1);
						else
							((Combo) criterion1Map.get(key)).select(2);
					}
					else
						((Combo) criterion1Map.get(key)).select(0);
				}
				else if (s.getDataType() == SearchFieldDataTypeEnum.ENUM) {
					final var tmp = (Combo) criterion1Map.get(key);
					tmp.setItems(s.getEnumListValues().values().toArray(new String[s.getEnumListValues().size()]));

					if (criteria != null)
						tmp.select(tmp.indexOf(s.getEnumListValues().get(criteria)));
				}
				else if (s.getLovCommand() != null) {
					final var tmp = (Text) criterion1Map.get(key);

					if (criteria != null)
						tmp.setText(criteria);
				}
				else {
					if (s.hasTemporalDataType()) {
						criterion1Map.get(key).addFocusListener(new FocusListener() {
							/*
							 * (non-Javadoc)
							 * @see org.eclipse.swt.events.FocusListener#focusLost(org.eclipse.swt.events.FocusEvent)
							 */
							@Override
							public void focusLost(FocusEvent event) {
								if (((Text) criterion1Map.get(key)).getText() != null && ((Text) criterion1Map.get(key)).getText().length() > 1)
									parseDate(((Text) criterion1Map.get(key)).getText(), s.getColLabel(), s.isDateTimeFormat());
							}

							/*
							 * (non-Javadoc)
							 * @see org.eclipse.swt.events.FocusListener#focusGained(org.eclipse.swt.events.FocusEvent)
							 */
							@Override
							public void focusGained(FocusEvent event) {
								// No implementation required!
							}
						});
					}

					if (criteria != null && !criteria.isEmpty()) {
						if (criteria.contains(TOKEN_DELIMITER_BETWEEN))
							((Text) criterion1Map.get(key)).setText(criteria.substring(0, criteria.indexOf(TOKEN_DELIMITER_BETWEEN)));
						else
							((Text) criterion1Map.get(key)).setText(criteria);
					}
				}

				if (c.getItem(c.getSelectionIndex()).equals(OPERATOR_IS_NULL)
						|| c.getItem(c.getSelectionIndex()).equals(OPERATOR_IS_NOT_NULL))
					criterion1Map.get(key).setVisible(false);

				addFeaturesToFieldLabel(lblCrit1, s);

				if (s.hasTemporalDataType()) {
					lblCrit1.addMouseListener(new MouseAdapter() {
						/*
						 * (non-Javadoc)
						 * @see org.eclipse.swt.events.MouseAdapter#mouseDoubleClick(org.eclipse.swt.events.MouseEvent)
						 */
						@Override
						public void mouseDown(MouseEvent e) {
							var date = new Date();

							if (((Text) criterion1Map.get(key)).getText() != null && ((Text) criterion1Map.get(key)).getText().length() > 1) {
								date = parseDate(((Text) criterion1Map.get(key)).getText(), s.getColLabel(), s.isDateTimeFormat());

								if (date == null)
									return;
							}

							final var dateTimeDialog = new DateTimePicker(getShell(), date, s.isDateTimeFormat());
							dateTimeDialog.open();

							if (dateTimeDialog.getReturnCode() == Dialog.OK) {
								if (s.isDateTimeFormat())
									((Text) criterion1Map.get(key)).setText(dateTimeFormat.format(dateTimeDialog.getDate()));
								else
									((Text) criterion1Map.get(key)).setText(dateFormat.format(dateTimeDialog.getDate()));
							}
						}
					});
				}
				else if (s.getListOfValues() != null) {
					// Add a listener for opening a list-of-values dialog
					lblCrit1.addMouseListener(new MouseAdapter() {
						/*
						 * (non-Javadoc)
						 * @see org.eclipse.swt.events.MouseAdapter#mouseDown(org.eclipse.swt.events.MouseEvent)
						 */
						@Override
						@SuppressWarnings("unchecked")
						public void mouseDown(MouseEvent e) {
							try {
								final var cl = (Class<__AbstractLOVDialog<?>>) Class.forName(s.getListOfValues());
								final var cons = (Constructor<__AbstractLOVDialog<?>>[]) cl.getConstructors();
								final Constructor<__AbstractLOVDialog<?>> constructor = cons[0];

								final var arguments = new Object[4];
								arguments[0] = thisShell;
								arguments[1] = ((Text) criterion1Map.get(key)).getText();
								arguments[2] = Boolean.valueOf(true);
								arguments[3] = Boolean.valueOf(false);

								final __AbstractLOVDialog<?> lov = constructor.newInstance(arguments);
								lov.open();

								if (lov.getReturnCode() == Dialog.OK && lov.getDisplayValue() != null) {
									((Text) criterion1Map.get(key)).setText(lov.getDisplayValue());

									// Check if the user has selected more than one item!
									if (lov.getDisplayValue().contains(TOKEN_DELIMITER_IN))
										s.setOperator(SearchOperatorHelper.getOperator(OPERATOR_IN));
									else
										s.setOperator(SearchOperatorHelper.getOperator(OPERATOR_EQUAL));

									final String selectedOperator = s.getOperator().getDescription();

									for (int i = 0; i < c.getItemCount(); i++) {
										if (c.getItem(i).equals(selectedOperator)) {
											c.select(i);
											break;
										}
									}
								}
							}
							catch (final Exception ex) {
								logger.error("Error while opening list of values dialog!", ex);

								setErrorMessage(getTranslation(SEARCH_INPUT_DIALOG_MSG_ERR_LOV) + ex.getMessage());
							}
						}
					});
				}

				final String operator = operatorMap.get(key).getItem(operatorMap.get(key).getSelectionIndex());

				if (operator.equals(OPERATOR_IS_NOT_NULL) || operator.equals(OPERATOR_IS_NULL))
					lblCrit1.setVisible(false);

				// Modify criteria field 2
				if (s.hasTemporalDataType()) {
					txtCrit2.addFocusListener(new FocusListener() {
						/*
						 * (non-Javadoc)
						 * @see org.eclipse.swt.events.FocusListener#focusLost(org.eclipse.swt.events.FocusEvent)
						 */
						@Override
						public void focusLost(FocusEvent event) {
							if (txtCrit2.getText() != null && txtCrit2.getText().length() > 1)
								parseDate(txtCrit2.getText(), s.getColLabel(), s.isDateTimeFormat());
						}

						/*
						 * (non-Javadoc)
						 * @see org.eclipse.swt.events.FocusListener#focusGained(org.eclipse.swt.events.FocusEvent)
						 */
						@Override
						public void focusGained(FocusEvent event) {
							// No implementation required!
						}
					});
				}

				if (s.getDataType() == SearchFieldDataTypeEnum.BOOLEAN)
					txtCrit2.setVisible(false);

				if (criteria != null && criteria.contains(TOKEN_DELIMITER_BETWEEN))
					txtCrit2.setText(criteria.substring(criteria.indexOf(TOKEN_DELIMITER_BETWEEN)).trim());

				if (s.getOperator() == null || !s.getOperator().getDescription().equals(OPERATOR_BETWEEN))
					txtCrit2.setVisible(false);

				addFeaturesToFieldLabel(lblCrit2, s);

				if (s.hasTemporalDataType()) {
					lblCrit2.addMouseListener(new MouseAdapter() {
						/*
						 * (non-Javadoc)
						 * @see org.eclipse.swt.events.MouseAdapter#mouseDoubleClick(org.eclipse.swt.events.MouseEvent)
						 */
						@Override
						public void mouseDown(MouseEvent e) {
							var date = new Date();

							if (txtCrit2.getText() != null && txtCrit2.getText().length() > 1) {
								date = parseDate(txtCrit2.getText(), s.getColLabel(), s.isDateTimeFormat());

								if (date == null)
									return;
							}

							final var dateTimeDialog = new DateTimePicker(getShell(), date, s.isDateTimeFormat());
							dateTimeDialog.open();

							if (dateTimeDialog.getReturnCode() == Dialog.OK) {
								if (s.isDateTimeFormat())
									txtCrit2.setText(dateTimeFormat.format(dateTimeDialog.getDate()));
								else
									txtCrit2.setText(dateFormat.format(dateTimeDialog.getDate()));
							}
						}
					});
				}

				if (!operator.equals(OPERATOR_BETWEEN))
					lblCrit2.setVisible(false);
			}

			panFilter.setSize(560, 2000);
			scrolledComposite.setContent(panFilter);

			int i = 0;

			for (final SortDirectionEnum sortOrder : SortDirectionEnum.values()) {
				sortOrders[i] = sortOrder.name();
				i++;
			}

			final var tabItemAdvancedSettings = new TabItem(tabFolder, SWT.NONE);
			tabItemAdvancedSettings.setText(getTranslation(SEARCH_INPUT_DIALOG_TAB_ADV_SETTINGS));

			final var panBasic = new Composite(tabFolder, SWT.NONE);
			panBasic.setLayout(new GridLayout(3, false));

			tabItemAdvancedSettings.setControl(panBasic);

			chkCase = new Button(panBasic, SWT.CHECK);
			chkCase.setText(getTranslation(SEARCH_INPUT_DIALOG_CHK_CASE));
			chkCase.setLayoutData(new GridData(127, SWT.DEFAULT));
			chkCase.setSelection(searchObj.isCaseSensitive());

			chkCount = new Button(panBasic, SWT.CHECK);
			chkCount.setLayoutData(new GridData(120, SWT.DEFAULT));
			chkCount.setSelection(searchObj.isCount());
			chkCount.setText(getTranslation(SEARCH_INPUT_DIALOG_CHK_COUNT));

			chkExact = new Button(panBasic, SWT.CHECK);
			chkExact.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, true, false));
			chkExact.setText(getTranslation(SEARCH_INPUT_DIALOG_CB_EXACT_FILTER_MATCH));
			chkExact.setSelection(searchObj.isExactFilterMatch());

			final var lblLimit = new Label(panBasic, SWT.NONE);
			lblLimit.setText(getTranslationForFieldLabel(SEARCH_INPUT_DIALOG_MAX_FETCH_SIZE));

			cboFetchSize = new Combo(panBasic, SWT.READ_ONLY);
			cboFetchSize.setLayoutData(new GridData(121, SWT.DEFAULT));

			// Add the fetch-size items to be selected by the user
			cboFetchSize.add("10");
			cboFetchSize.add("100");
			cboFetchSize.add("1000");
			cboFetchSize.add("10000");
			cboFetchSize.add("100000");
			cboFetchSize.add("1000000");

			int selIndex = -1;

			for (final String item : cboFetchSize.getItems()) {
				selIndex++;

				final int value = Integer.parseInt(item);

				if (value == searchObj.getMaxResult())
					break;
			}

			if (selIndex != -1)
				cboFetchSize.select(selIndex);
			else
				cboFetchSize.select(0);

			new Label(panBasic, SWT.NONE);

			final var panFields = new Group(panBasic, SWT.NONE);
			panFields.setLayout(new GridLayout(3, false));
			panFields.setText(getTranslationForFieldLabel(SEARCH_INPUT_DIALOG_VISUAL_FIELDS));
			panFields.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 3, 1));

			final var lblAvailableFields = new Label(panFields, SWT.NONE);
			lblAvailableFields.setText(getTranslationForFieldLabel(SEARCH_INPUT_DIALOG_AVAILABLE_FIELDS));

			new Label(panFields, SWT.NONE);

			final var lblSelFields = new Label(panFields, SWT.NONE);
			lblSelFields.setText(getTranslationForFieldLabel(SEARCH_INPUT_DIALOG_SEL_FIELDS));

			tableViewerAllFields = new TableViewer(panFields, SWT.V_SCROLL | SWT.H_SCROLL | SWT.BORDER);

			tableAllFields = tableViewerAllFields.getTable();
			tableAllFields.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
			tableAllFields.setHeaderVisible(false);
			tableAllFields.setLinesVisible(false);

			tableViewerAllFields.addDoubleClickListener(_ -> selectField());
			tableViewerAllFields.setComparator(new ColLabelComparator());
			tableViewerAllFields.setLabelProvider(new FieldLabelProvider());
			tableViewerAllFields.setContentProvider(new AllFieldsContentProvider());
			tableViewerAllFields.setInput(searchObj.getSearchFields());

			final var colTableAllFields = new TableColumn(tableAllFields, SWT.NONE);
			colTableAllFields.setWidth(200);

			final var gdButtons = new GridData(SWT.CENTER, SWT.CENTER, false, false);
			gdButtons.minimumWidth = 60;

			final var panButtonsCols = new Composite(panFields, SWT.NONE);
			panButtonsCols.setLayoutData(gdButtons);
			panButtonsCols.setLayout(new GridLayout());

			final var cmdSelField = new Button(panButtonsCols, SWT.NONE);

			cmdSelField.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(final SelectionEvent e) {
					selectField();
				}
			});

			cmdSelField.setImage(ImageCache.getImage(ImageCache.IMG_SELECT));

			final var cmdDeSelectField = new Button(panButtonsCols, SWT.NONE);

			cmdDeSelectField.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(final SelectionEvent e) {
					deSelectField();
				}
			});

			cmdDeSelectField.setImage(ImageCache.getImage(ImageCache.IMG_DESELECT));

			tableViewerSelFields = new TableViewer(panFields, SWT.V_SCROLL | SWT.H_SCROLL | SWT.BORDER);
			tableViewerSelFields.addDoubleClickListener(_ -> deSelectField());
			tableViewerSelFields.setLabelProvider(new FieldLabelProvider());
			tableViewerSelFields.setContentProvider(new SelectedFieldsContentProvider());
			tableViewerSelFields.setComparator(new ColOrderComparator());
			tableViewerSelFields.setInput(searchObj.getSearchFields());

			tableSelFields = tableViewerSelFields.getTable();
			tableSelFields.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
			tableSelFields.setToolTipText(getTranslation(SEARCH_INPUT_DIALOG_TOOLTIP_UP_DOWN));

			tableSelFields.addKeyListener(new KeyAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.KeyAdapter#keyPressed(org.eclipse.swt.events.KeyEvent)
				 */
				@Override
				public void keyPressed(KeyEvent e) {
					if (e.character == '+')
						moveFieldUp();

					if (e.character == '-')
						moveFieldDown();
				}
			});

			final var colTableSelFields = new TableColumn(tableSelFields, SWT.NONE);
			colTableSelFields.setWidth(200);

			final var menu = new Menu(tableSelFields);
			tableSelFields.setMenu(menu);

			final var mniMoveFieldUp = new MenuItem(menu, SWT.NONE);

			mniMoveFieldUp.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(SelectionEvent e) {
					moveFieldUp();
				}
			});

			mniMoveFieldUp.setText(getTranslation(SEARCH_INPUT_DIALOG_CMD_MOVE_UP));
			mniMoveFieldUp.setImage(ImageCache.getImage(ImageCache.IMG_MOVE_UP));

			final var mniMoveFieldDown = new MenuItem(menu, SWT.NONE);

			mniMoveFieldDown.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(SelectionEvent e) {
					moveFieldDown();
				}
			});

			mniMoveFieldDown.setText(getTranslation(SEARCH_INPUT_DIALOG_CMD_MOVE_DOWN));
			mniMoveFieldDown.setImage(ImageCache.getImage(ImageCache.IMG_MOVE_DOWN));

			// Allow data to be copied or moved from the drag source
			int operations = DND.DROP_MOVE | DND.DROP_COPY;
			final var source = new DragSource(tableSelFields, operations);

			// Provide the data in text format
			source.setTransfer(TextTransfer.getInstance());

			source.addDragListener(new DragSourceListener() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.dnd.DragSourceListener#dragStart(org.eclipse.swt.dnd.DragSourceEvent)
				 */
				@Override
				public void dragStart(DragSourceEvent event) {
					// Only start the drag operation if a field is selected
					final var selection = (IStructuredSelection) tableViewerSelFields.getSelection();

					if (selection.getFirstElement() == null)
						event.doit = false;

					event.doit = true;
				}

				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.dnd.DragSourceListener#dragSetData(org.eclipse.swt.dnd.DragSourceEvent)
				 */
				@Override
				public void dragSetData(DragSourceEvent event) {
					// If a field is selected the event data should be provided with the name of the column
					final var selection = (IStructuredSelection) tableViewerSelFields.getSelection();
					final var field = (SearchFieldDTO) selection.getFirstElement();

					if (field == null)
						return;

					event.data = field.getColName();
				}

				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.dnd.DragSourceListener#dragFinished(org.eclipse.swt.dnd.DragSourceEvent)
				 */
				@Override
				public void dragFinished(DragSourceEvent event) {
					// No implementation required!
				}
			});

			final var lblSort = new Label(panFields, SWT.NONE);
			lblSort.setText(getTranslationForFieldLabel(SEARCH_INPUT_DIALOG_LBL_SORT));

			new Label(panFields, SWT.NONE);
			new Label(panFields, SWT.NONE);

			tableViewerSorting = new TableViewer(panFields, SWT.V_SCROLL | SWT.H_SCROLL | SWT.BORDER);
			tableViewerSorting.setLabelProvider(new SortLabelProvider());
			tableViewerSorting.setContentProvider(new SortContentProvider());
			tableViewerSorting.setComparator(new SortComparator());
			tableViewerSorting.setInput(searchObj.getSearchFields());

			final var gdSort = new GridData(SWT.FILL, SWT.FILL, true, false, 3, 1);
			gdSort.minimumHeight = 80;

			tableSorting = tableViewerSorting.getTable();
			tableSorting.setLayoutData(gdSort);

			final var menuSort = new Menu(tableSorting);
			tableSorting.setMenu(menuSort);

			final var mniEditSortField = new MenuItem(menuSort, SWT.NONE);
			mniEditSortField.setText(getTranslation(SEARCH_INPUT_DIALOG_CMD_INV_SORT));

			mniEditSortField.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(SelectionEvent e) {
					invertSortOrder();
				}
			});

			final var mniMoveSortFieldUp = new MenuItem(menuSort, SWT.NONE);
			mniMoveSortFieldUp.setText(getTranslation(SEARCH_INPUT_DIALOG_CMD_MOVE_UP));
			mniMoveSortFieldUp.setImage(ImageCache.getImage(ImageCache.IMG_MOVE_UP));

			mniMoveSortFieldUp.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(SelectionEvent e) {
					moveSortFieldUp();
				}
			});

			final var mniMoveSortFieldDown = new MenuItem(menuSort, SWT.NONE);
			mniMoveSortFieldDown.setText(getTranslation(SEARCH_INPUT_DIALOG_CMD_MOVE_DOWN));
			mniMoveSortFieldDown.setImage(ImageCache.getImage(ImageCache.IMG_MOVE_DOWN));

			mniMoveSortFieldDown.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(SelectionEvent e) {
					moveSortFieldDown();
				}
			});

			final var mniRemoveSortField = new MenuItem(menuSort, SWT.NONE);
			mniRemoveSortField.setText(getTranslation(SEARCH_INPUT_DIALOG_CMD_REMOVE_SORT_ITEM));

			mniRemoveSortField.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(SelectionEvent e) {
					removeSortField();
				}
			});

			final var mniRemoveAllSortFields = new MenuItem(menuSort, SWT.NONE);
			mniRemoveAllSortFields.setText(getTranslation(SEARCH_INPUT_DIALOG_CMD_REMOVE_SORT_ITEMS));

			mniRemoveAllSortFields.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(SelectionEvent e) {
					removeAllSortFields();
				}
			});

			// Allow data to be copied or moved to the drop target
			operations = DND.DROP_MOVE | DND.DROP_COPY | DND.DROP_DEFAULT;
			final var target = new DropTarget(tableSorting, operations);

			// Receive data in Text or File format
			final TextTransfer textTransfer = TextTransfer.getInstance();
			target.setTransfer(textTransfer);

			target.addDropListener(new DropTargetAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.dnd.DropTargetAdapter#dragEnter(org.eclipse.swt.dnd.DropTargetEvent)
				 */
				@Override
				public void dragEnter(DropTargetEvent event) {
					// No implementation required!
				}

				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.dnd.DropTargetAdapter#dragOver(org.eclipse.swt.dnd.DropTargetEvent)
				 */
				@Override
				public void dragOver(DropTargetEvent event) {
					event.feedback = DND.FEEDBACK_SELECT | DND.FEEDBACK_SCROLL;
				}

				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.dnd.DropTargetAdapter#dragOperationChanged(org.eclipse.swt.dnd.DropTargetEvent)
				 */
				@Override
				public void dragOperationChanged(DropTargetEvent event) {
					// No implementation required!
				}

				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.dnd.DropTargetAdapter#drop(org.eclipse.swt.dnd.DropTargetEvent)
				 */
				@Override
				public void drop(DropTargetEvent event) {
					int sortIndex = 0;
					int maxSortIndex = 0;

					// Test if this field has been already selected
					for (final SearchFieldDTO f : searchObj.getSearchFields())
						if (f.getColName().equals(event.data) && f.getSortIndex() != 0)
							return;

					// Get the max. sort index
					for (final SearchFieldDTO f : searchObj.getSearchFields()) {
						if (f.getSortIndex() == 0)
							continue;

						if (f.getSortIndex() > maxSortIndex)
							maxSortIndex = f.getSortIndex();
					}

					if (maxSortIndex == 0)
						sortIndex = 1;
					else
						sortIndex = maxSortIndex + 1;

					for (final SearchFieldDTO f : searchObj.getSearchFields())
						if (f.getColName().equals(event.data)) {
							f.setSortIndex(sortIndex);
							f.setSortOrder(SortDirectionEnum.ASC);
						}

					tableViewerSorting.setInput(searchObj.getSearchFields());
				}
			});

			final var colTableSortFields = new TableColumn(tableSorting, SWT.NONE);
			colTableSortFields.setWidth(250);

			setMessage(getTranslation(SEARCH_INPUT_DIALOG_TITLE_MSG));
			setTitle(DIALOG_TITLE);
			setTitleImage(ImageCache.getImage(ImageCache.IMG_TITLE_SEARCH));

			return panDialogArea;
		}
		catch (final Exception e) {
			logger.error("Could not initialize dialog!", e);

			return null;
		}
	}

	/**
	 * Move the field down
	 */
	protected void moveFieldDown() {
		final var sel = (IStructuredSelection) tableViewerSelFields.getSelection();
		final var field = (SearchFieldDTO) sel.getFirstElement();

		if (field == null)
			return;

		if (field.getColOrder() < rowCount) {
			for (final SearchFieldDTO thisField : searchObj.getSearchFields()) {
				if (thisField.getColOrder() == (field.getColOrder() + 1)) {
					thisField.setColOrder(thisField.getColOrder() - 1);
					break;
				}
			}

			// If the field is already the last the column order must not be changed!
			if (field.getColOrder() + 1 < rowCount)
				field.setColOrder(field.getColOrder() + 1);

			final var newSel = new StructuredSelection(field);

			tableViewerSelFields.setInput(searchObj.getSearchFields());
			tableViewerSelFields.setSelection(newSel);
		}
	}

	/**
	 * Move the field up
	 */
	protected void moveFieldUp() {
		final var sel = (IStructuredSelection) tableViewerSelFields.getSelection();
		final var field = (SearchFieldDTO) sel.getFirstElement();

		if (field == null)
			return;

		if (field.getColOrder() > 0) {
			for (final SearchFieldDTO thisField : searchObj.getSearchFields()) {
				if (thisField.getColOrder() == (field.getColOrder() - 1)) {
					thisField.setColOrder(thisField.getColOrder() + 1);
					break;
				}
			}

			field.setColOrder(field.getColOrder() - 1);
		}

		final var newSel = new StructuredSelection(field);

		tableViewerSelFields.setInput(searchObj.getSearchFields());
		tableViewerSelFields.setSelection(newSel);
	}

	/**
	 * Move the sort field up
	 */
	protected void moveSortFieldUp() {
		final var sel = (StructuredSelection) tableViewerSorting.getSelection();
		final var field = (SearchFieldDTO) sel.getFirstElement();

		if (field == null)
			return;

		final int sortIndex = field.getSortIndex();

		if (sortIndex == 1)
			return;

		field.setSortIndex(sortIndex - 1);

		for (final SearchFieldDTO f : searchObj.getSearchFields())
			if (sortIndex == (f.getSortIndex() + 1) && !f.getColName().equals(field.getColName()))
				f.setSortIndex(f.getSortIndex() + 1);

		tableViewerSorting.setInput(searchObj.getSearchFields());
	}

	/**
	 * Move the sort field down
	 */
	protected void moveSortFieldDown() {
		final var sel = (StructuredSelection) tableViewerSorting.getSelection();
		final var field = (SearchFieldDTO) sel.getFirstElement();

		if (field == null)
			return;

		int sortFieldCount = 0;

		// Get the number of sort fields
		for (final SearchFieldDTO f : searchObj.getSearchFields())
			if (f.getSortIndex() > 0)
				sortFieldCount++;

		final int sortIndex = field.getSortIndex();

		if (sortIndex == sortFieldCount)
			return;

		field.setSortIndex(sortIndex + 1);

		for (final SearchFieldDTO f : searchObj.getSearchFields())
			if (sortIndex == (f.getSortIndex() - 1) && !f.getColName().equals(field.getColName()))
				f.setSortIndex(f.getSortIndex() - 1);

		tableViewerSorting.setInput(searchObj.getSearchFields());
	}

	/**
	 * Remove the field from the sort list
	 */
	protected void removeSortField() {
		final var sel = (StructuredSelection) tableViewerSorting.getSelection();
		final var field = (SearchFieldDTO) sel.getFirstElement();

		if (field == null)
			return;

		final int sortIndex = field.getSortIndex();
		field.setSortIndex(0);
		field.setSortOrder(SortDirectionEnum.NONE);

		for (final SearchFieldDTO f : searchObj.getSearchFields())
			if (f.getSortIndex() > sortIndex)
				f.setSortIndex(f.getSortIndex() - 1);

		tableViewerSorting.setInput(searchObj.getSearchFields());
	}

	/**
	 * Remove all fields from the sort list
	 */
	protected void removeAllSortFields() {
		searchObj.getSearchFields().forEach(f -> {
			f.setSortIndex(0);
			f.setSortOrder(SortDirectionEnum.NONE);
		});

		tableViewerSorting.setInput(searchObj.getSearchFields());
	}

	/**
	 * Invert the sort order direction of the selected field
	 */
	protected void invertSortOrder() {
		final var sel = (StructuredSelection) tableViewerSorting.getSelection();
		final var field = (SearchFieldDTO) sel.getFirstElement();

		if (field == null)
			return;

		if (field.getSortOrder() == SortDirectionEnum.ASC)
			field.setSortOrder(SortDirectionEnum.DESC);
		else
			field.setSortOrder(SortDirectionEnum.ASC);

		tableViewerSorting.setInput(searchObj.getSearchFields());
	}

	/**
	 * Move the selected field to the list that contains all visible fields
	 */
	protected void selectField() {
		final var sel = (StructuredSelection) tableViewerAllFields.getSelection();
		final var field = (SearchFieldDTO) sel.getFirstElement();

		if (field == null)
			return;

		// Get the last visible column
		int lastVisibleIndex = 0;

		for (final SearchFieldDTO f : searchObj.getSearchFields())
			if (f.isVisible() && f.getColOrder() > lastVisibleIndex)
				lastVisibleIndex = f.getColOrder();

		// Test if the field is already selected
		for (final SearchFieldDTO f : searchObj.getSearchFields())
			if (f.getColName().equals(field.getColName())) {
				if (f.isVisible())
					return;

				// Change the index of all invisible fields
				for (final SearchFieldDTO fi : searchObj.getSearchFields())
					if (!fi.isVisible() && fi.getColOrder() < f.getColOrder())
						fi.setColOrder(fi.getColOrder() + 1);

				f.setColOrder(lastVisibleIndex + 1);
				f.setVisible(true);

				tableViewerSelFields.setInput(searchObj.getSearchFields());
				tableViewerAllFields.setInput(searchObj.getSearchFields());
			}
	}

	/**
	 * Move the selected field to the list that contains all available fields
	 */
	protected void deSelectField() {
		final var sel = (StructuredSelection) tableViewerSelFields.getSelection();
		final var field = (SearchFieldDTO) sel.getFirstElement();

		if (field == null)
			return;

		int visualColCount = 0;

		// Check the minimum number of visible fields
		for (final SearchFieldDTO f : searchObj.getSearchFields())
			if (f.isVisible())
				visualColCount++;

		if (visualColCount == MIN_VISUAL_FIELD_COUNT) {
			MessageDialog.openInformation(thisShell, DIALOG_TITLE,
					getTranslation(SEARCH_INPUT_DIALOG_MSG_ERR_MIN_FIELD_COUNT, MIN_VISUAL_FIELD_COUNT));
			return;
		}

		// Test if a field is already selected
		for (final SearchFieldDTO f : searchObj.getSearchFields())
			if (f.getColName().equals(field.getColName())) {
				if (!f.isVisible())
					return;

				// Change the order of all other columns below the selected column! Make sure that all invisible fields are placed on end
				// of the list.
				for (final SearchFieldDTO fi : searchObj.getSearchFields())
					if (fi.getColOrder() > f.getColOrder())
						fi.setColOrder(fi.getColOrder() - 1);

				// Move the field to the bottom
				f.setColOrder(searchObj.getSearchFields().size() - 1);
				f.setVisible(false);

				if (f.getSortIndex() > 0) {
					final int sortIndex = f.getSortIndex();
					f.setSortIndex(0);
					f.setSortOrder(SortDirectionEnum.NONE);

					for (final SearchFieldDTO s : searchObj.getSearchFields())
						if (s.getSortIndex() > sortIndex)
							s.setSortIndex(s.getSortIndex() - 1);

					// Update the sorting table
					tableViewerSorting.setInput(searchObj.getSearchFields());
				}

				// Update the table that contains the visible fields
				tableViewerSelFields.setInput(searchObj.getSearchFields());
				tableViewerAllFields.setInput(searchObj.getSearchFields());
			}
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		createButton(parent, IDialogConstants.OK_ID, getTranslation(SEARCH_INPUT_DIALOG_CMD_SEARCH), true);
		createButton(parent, IDialogConstants.DETAILS_ID, getTranslation(SEARCH_INPUT_DIALOG_CMD_COUNT), false);
		createButton(parent, IDialogConstants.DESELECT_ALL_ID, getTranslation(SEARCH_INPUT_DIALOG_CMD_CLEAR), false);
		createButton(parent, IDialogConstants.CANCEL_ID, getTranslation(CMD_CANCEL), false);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.TitleAreaDialog#getInitialSize()
	 */
	@Override
	protected Point getInitialSize() {
		int height = DIALOG_MIN_HEIGHT;
		final int resolution = Display.getDefault().getBounds().height;

		// The maximum height of the dialog should be smaller than the height of the screen
		final int maxHeight = (int) (resolution - (resolution * 0.3));
		int numberOfRows = 0;

		for (final SearchFieldDTO f : searchObj.getSearchFields())
			if (f.getType() == SearchFieldTypeEnum.STANDARD)
				numberOfRows++;

		final int calcHeight = numberOfRows * ROW_HEIGHT_FACTOR + DIALOG_HEIGHT_OFFSET;

		// If the calculated size is greater than the screen resolution the dialog height must be reduced!
		if (calcHeight > maxHeight)
			height = maxHeight;
		else if (calcHeight < maxHeight && calcHeight > DIALOG_MIN_HEIGHT)
			height = calcHeight;

		return new Point(620, height);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
	 */
	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);
		newShell.setText(DIALOG_TITLE);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		// Reset the error message
		setErrorMessage(null);

		if (buttonId == IDialogConstants.OK_ID) {
			if (!initializeSearchObject(false))
				return;
		}
		else if (buttonId == IDialogConstants.DETAILS_ID) {
			if (!initializeSearchObject(false))
				return;

			// Delegate the counting of records to the view that opened this dialog!
			final Cursor defaultCursor = Display.getCurrent().getActiveShell().getCursor();
			Display.getCurrent().getActiveShell().setCursor(Display.getCurrent().getSystemCursor(SWT.CURSOR_WAIT));

			long countResult = 0;

			try {
				countResult = countable.countData();
			}
			catch (final Exception e) {
				logger.error("Error while performing count operation!", e);

				Display.getCurrent().getActiveShell().setCursor(defaultCursor);
				MessageDialog.openError(thisShell, DIALOG_TITLE, getTranslation(SEARCH_INPUT_DIALOG_MSG_COUNT_ERROR) + e.getMessage());
				return;
			}

			Display.getCurrent().getActiveShell().setCursor(defaultCursor);

			MessageDialog.openInformation(thisShell, DIALOG_TITLE, getTranslation(SEARCH_INPUT_DIALOG_MSG_COUNT_RES, countResult));
			return;
		}
		else if (buttonId == IDialogConstants.DESELECT_ALL_ID) {
			if (!initializeSearchObject(true))
				return;

			tableViewerSelFields.setInput(searchObj.getSearchFields());
			tableViewerSorting.setInput(searchObj.getSearchFields());
			return;
		}

		super.buttonPressed(buttonId);
	}

	/**
	 * Add further features to the given text input field
	 * @param textInputField
	 * @param cboOperator
	 * @param searchField
	 */
	@SuppressWarnings("unused")
	protected void addFeaturesTextInputField(Text textInputField, Combo cboOperator, SearchFieldDTO searchField) {

	}

	/**
	 * Add further features to the given label
	 * @param label
	 * @param searchField
	 */
	@SuppressWarnings("unused")
	protected void addFeaturesToFieldLabel(Label label, SearchFieldDTO searchField) {

	}

	/**
	 * Validate the user input and initialize the search object
	 * @param reset if parameter is set to true no validation will be performed! Search object will be reset to default values!
	 * @return true if the initialization was successful
	 */
	private boolean initializeSearchObject(boolean reset) {
		final int maxResult = Integer.parseInt(cboFetchSize.getItem(cboFetchSize.getSelectionIndex()));

		if (reset) {
			// Reset the filter criteria and the operators of all search fields
			for (final SearchFieldDTO field : searchObj.getSearchFields()) {
				if (field.getType() == SearchFieldTypeEnum.NOT_SEARCHABLE)
					continue;

				field.setFilterCriteria("");
				field.setOperator(null);
				field.setVisible(true);
				field.setSortIndex(0);
				field.setSortOrder(SortDirectionEnum.NONE);

				final String key = field.getColLabel();

				// Reset the corresponding input fields
				if (operatorMap.containsKey(key)) {
					operatorMap.get(key).select(0);

					if (field.getDataType() == SearchFieldDataTypeEnum.BOOLEAN)
						((Combo) criterion1Map.get(key)).select(0);
					else if (field.getDataType() == SearchFieldDataTypeEnum.ENUM)
						((Combo) criterion1Map.get(key)).setText("");
					else
						((Text) criterion1Map.get(key)).setText("");

					criterion2Map.get(key).setText("");
					criterion2Map.get(key).setVisible(false);
					labelCrit2Map.get(key).setVisible(false);
				}
			}

			return true;
		}

		// Save important field information temporarily in order to prevent saving illegal field data if the validation will fail!
		final var tempFieldList = new ArrayList<SearchFieldDTO>();

		for (final SearchFieldDTO searchField : searchObj.getSearchFields()) {
			if (searchField.getType() == SearchFieldTypeEnum.NOT_SEARCHABLE)
				continue;

			final String key = searchField.getColLabel();

			final var tempField = new SearchFieldDTO();
			tempField.setDataType(searchField.getDataType());
			tempField.setColLabel(searchField.getColLabel());
			tempField.setDateTimeFormat(searchField.isDateTimeFormat());
			tempField.setVisible(searchField.isVisible());
			tempField.setListOfValues(searchField.getListOfValues());
			tempField.setType(searchField.getType());
			tempField.setEnumListValues(searchField.getEnumListValues());

			tempFieldList.add(tempField);

			if (operatorMap.containsKey(key)) {
				final String op = operatorMap.get(key).getItem(operatorMap.get(key).getSelectionIndex());

				if (op.isEmpty())
					tempField.setOperator(null);
				else {
					for (final SearchOperatorDTO sod : SearchOperatorHelper.getAllOperators())
						if (op.equals(sod.getDescription())) {
							tempField.setOperator(sod);
							break;
						}
				}

				if (tempField.getDataType() == SearchFieldDataTypeEnum.BOOLEAN) {
					final var tmp = (Combo) criterion1Map.get(key);

					if (tmp.getItem(tmp.getSelectionIndex()).isEmpty())
						tempField.setFilterCriteria(null);
					else
						tempField.setFilterCriteria(tmp.getItem(tmp.getSelectionIndex()));
				}
				else if (tempField.getDataType() == SearchFieldDataTypeEnum.ENUM) {
					final var tmp = (Combo) criterion1Map.get(key);

					if (tmp.getText().isEmpty())
						tempField.setFilterCriteria(null);
					else
						for (final String enumLiteral : tempField.getEnumListValues().keySet())
							if (tempField.getEnumListValues().get(enumLiteral).equals(tmp.getText()))
								tempField.setFilterCriteria(enumLiteral);
				}
				else if (tempField.getLovCommand() != null) {
					final var tmp = (Text) criterion1Map.get(key);

					if (tmp.getText().isEmpty())
						tempField.setFilterCriteria(null);
					else
						tempField.setFilterCriteria(tmp.getText());
				}
				else {
					final SearchOperatorDTO tmpOperator = tempField.getOperator();

					if (tmpOperator != null) {
						if (tmpOperator.isExpectsArgument()) {
							tempField.setFilterCriteria(((Text) criterion1Map.get(key)).getText());

							if (tmpOperator.getDescription().equals(OPERATOR_BETWEEN))
								tempField.setFilterCriteria(
										tempField.getFilterCriteria() + TOKEN_DELIMITER_BETWEEN + (criterion2Map.get(key)).getText());
						}
						else
							tempField.setFilterCriteria(null);
					}
					else
						tempField.setFilterCriteria(((Text) criterion1Map.get(key)).getText());
				}
			}
		}

		// Perform the input validation on the temporary fields
		for (final SearchFieldDTO field : tempFieldList) {
			if (field.getType() == SearchFieldTypeEnum.NOT_SEARCHABLE)
				continue;

			// Check if the operator requires a filter criterion
			if (field.getOperator() != null && field.getOperator().isExpectsArgument()
					&& (field.getFilterCriteria() == null || field.getFilterCriteria().isEmpty())) {
				setErrorMessage(getTranslation(SEARCH_INPUT_DIALOG_MSG_ERR_CRITERION_EXP, field.getColLabel().toLowerCase()));
				return false;
			}

			// Validate the input of a date field
			if (field.hasTemporalDataType()) {
				String[] dateValues;

				if (field.getOperator() != null) {
					if (!field.getOperator().isExpectsArgument())
						dateValues = new String[0];
					else if (field.getOperator().getValue().equals(OPERATOR_BETWEEN)) {
						if (TOKEN_DELIMITER_BETWEEN_PATTERN.split(field.getFilterCriteria()).length != 2) {
							setErrorMessage(getTranslation(SEARCH_INPUT_DIALOG_MSG_ERR_MISSING_BETWEEN));
							return false;
						}

						dateValues = TOKEN_DELIMITER_BETWEEN_PATTERN.split(field.getFilterCriteria());
					}
					else {
						dateValues = new String[1];
						dateValues[0] = field.getFilterCriteria();
					}
				}
				else {
					dateValues = new String[1];
					dateValues[0] = field.getFilterCriteria();
				}

				// Check if date values have a valid format!
				for (final String dateValue : dateValues) {
					if (field.getOperator() != null && !field.getOperator().isExpectsArgument())
						continue;

					if (field.getOperator() == null && dateValue.isEmpty())
						continue;

					final Date date = parseDate(dateValue, field.getColLabel(), field.isDateTimeFormat());

					if (date == null)
						return false;
				}
			}

			// Validate the input of either a numeric or a UUID field
			if ((field.getDataType() == SearchFieldDataTypeEnum.LONG || field.getDataType() == SearchFieldDataTypeEnum.INTEGER
					|| field.getDataType() == SearchFieldDataTypeEnum.FLOAT || field.getDataType() == SearchFieldDataTypeEnum.DOUBLE
					|| field.getDataType() == SearchFieldDataTypeEnum.UUID_BINARY
					|| field.getDataType() == SearchFieldDataTypeEnum.UUID_STRING
					|| field.getDataType() == SearchFieldDataTypeEnum.BIG_DECIMAL) && field.getFilterCriteria() != null
					&& !field.getFilterCriteria().isEmpty()) {
				final String criteria = field.getFilterCriteria();
				String[] values;

				if (field.getOperator() != null) {
					if (field.getOperator().getValue().equals(OPERATOR_BETWEEN)) {
						values = TOKEN_DELIMITER_BETWEEN_PATTERN.split(criteria);

						if (values.length != 2) {
							setErrorMessage(getTranslation(SEARCH_INPUT_DIALOG_MSG_ERR_MISSING_BETWEEN));
							return false;
						}
					}
					else if (field.getOperator().getValue().equals(OPERATOR_IN) || field.getOperator().getValue().equals(OPERATOR_NOT_IN)) {
						values = TOKEN_DELIMITER_IN_PATTERN.split(criteria);

						if (values.length < 2) {
							setErrorMessage(getTranslation(SEARCH_INPUT_DIALOG_MSG_ERR_MISSING_IN));
							return false;
						}
					}
					else {
						values = new String[1];
						values[0] = criteria;
					}
				}
				else {
					values = new String[1];
					values[0] = criteria;
				}

				for (final String value : values) {
					try {
						if (field.getDataType() == SearchFieldDataTypeEnum.UUID_BINARY
								|| field.getDataType() == SearchFieldDataTypeEnum.UUID_STRING) {
							boolean checkInput = false;

							if (field.getOperator() == null && field.getDataType() == SearchFieldDataTypeEnum.UUID_BINARY)
								checkInput = true;

							if (field.getOperator() != null && !field.getOperator().getValue().equals(OPERATOR_LIKE)
									&& !field.getOperator().getValue().equals(OPERATOR_NOT_LIKE))
								checkInput = true;

							if (checkInput)
								UUID.fromString(value);
						}
						else if (field.getDataType() == SearchFieldDataTypeEnum.LONG
								|| field.getDataType() == SearchFieldDataTypeEnum.INTEGER)
							Long.parseLong(value);
						else
							decimalFormat.parse(value);
					}
					catch (final NumberFormatException | ParseException _) {
						setErrorMessage(getTranslation(SEARCH_INPUT_DIALOG_MSG_ERR_NO_NUMBER, field.getColLabel().toLowerCase()));
						return false;
					}
					catch (final IllegalArgumentException _) {
						setErrorMessage(getTranslation(SEARCH_INPUT_DIALOG_MSG_ERR_NO_UUID, field.getColLabel().toLowerCase()));
						return false;
					}
				}
			}
		}

		// Set filter criteria and operators
		for (final SearchFieldDTO sfd : searchObj.getSearchFields()) {
			if (sfd.getType() == SearchFieldTypeEnum.NOT_SEARCHABLE)
				continue;

			final String key = sfd.getColLabel();

			if (operatorMap.containsKey(key)) {
				final String op = operatorMap.get(key).getItem(operatorMap.get(key).getSelectionIndex());

				if (op.isEmpty())
					sfd.setOperator(null);
				else
					for (final SearchOperatorDTO sod : SearchOperatorHelper.getAllOperators())
						if (op.equals(sod.getDescription())) {
							sfd.setOperator(sod);
							break;
						}

				if (sfd.getDataType() == SearchFieldDataTypeEnum.BOOLEAN) {
					final var tmp = (Combo) criterion1Map.get(key);

					if (tmp.getItem(tmp.getSelectionIndex()).isEmpty())
						sfd.setFilterCriteria(null);
					else
						sfd.setFilterCriteria(tmp.getItem(tmp.getSelectionIndex()));
				}
				else if (sfd.getDataType() == SearchFieldDataTypeEnum.ENUM) {
					final var tmp = (Combo) criterion1Map.get(key);

					if (tmp.getText().isEmpty())
						sfd.setFilterCriteria(null);
					else
						for (final String enumLiteral : sfd.getEnumListValues().keySet())
							if (sfd.getEnumListValues().get(enumLiteral).equals(tmp.getText()))
								sfd.setFilterCriteria(enumLiteral);
				}
				else if (sfd.getLovCommand() != null) {
					final var tmp = (Text) criterion1Map.get(key);

					if (tmp.getText().isEmpty())
						sfd.setFilterCriteria(null);
					else
						sfd.setFilterCriteria(tmp.getText());
				}
				else {
					final SearchOperatorDTO tmpOperator = sfd.getOperator();

					if (tmpOperator != null) {
						if (tmpOperator.isExpectsArgument()) {
							sfd.setFilterCriteria(((Text) criterion1Map.get(key)).getText());

							if (tmpOperator.getDescription().equals(OPERATOR_BETWEEN))
								sfd.setFilterCriteria(sfd.getFilterCriteria() + TOKEN_DELIMITER_BETWEEN + (criterion2Map.get(key)).getText());
						}
						else
							sfd.setFilterCriteria(null);
					}
					else
						sfd.setFilterCriteria(((Text) criterion1Map.get(key)).getText());
				}
			}
		}

		searchObj.setMaxResult(maxResult);
		searchObj.setCaseSensitive(chkCase.getSelection());
		searchObj.setCount(chkCount.getSelection());
		searchObj.setExactFilterMatch(chkExact.getSelection());

		return true;
	}

	/**
	 * Convert the given string into a {@link Date}
	 * @param dateString
	 * @param fieldLabel
	 * @param useDateTimeFormat
	 * @return a {@link Date} object or null if the parsing has failed
	 */
	private Date parseDate(String dateString, String fieldLabel, boolean useDateTimeFormat) {
		final String pattern;

		if (useDateTimeFormat)
			pattern = userFormat.getDateTimeFormat();
		else
			pattern = userFormat.getDateFormat();

		try {
			if (useDateTimeFormat)
				return dateTimeFormat.parse(dateString);

			return dateFormat.parse(dateString);
		}
		catch (final ParseException _) {
			MessageDialog.openInformation(getParentShell(), DIALOG_TITLE,
					getTranslation(SEARCH_INPUT_DIALOG_MSG_ERR_DATE_FORMAT, fieldLabel, pattern));
			return null;
		}
	}

}
