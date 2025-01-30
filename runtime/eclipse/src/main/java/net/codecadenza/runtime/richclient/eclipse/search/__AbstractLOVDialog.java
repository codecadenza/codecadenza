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

import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.ABSTRACT_LOV_DIALOG_CMD_DESELECT;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.ABSTRACT_LOV_DIALOG_CMD_DESELECT_ALL;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.ABSTRACT_LOV_DIALOG_CMD_SEARCH;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.ABSTRACT_LOV_DIALOG_CMD_SELECT_ALL;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.ABSTRACT_LOV_DIALOG_LBL_FILTER;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.ABSTRACT_LOV_DIALOG_MSG_TITLE;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.CMD_CANCEL;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.CMD_OK;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.DATA_FETCH_ACTION_MSG_QUERY_FAILED;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.DATA_FETCH_ACTION_RESULT_NO_COUNT;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.getTranslation;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.getTranslationForFieldLabel;

import java.lang.invoke.MethodHandles;
import java.text.DecimalFormat;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import net.codecadenza.runtime.richclient.eclipse.image.ImageCache;
import net.codecadenza.runtime.richclient.eclipse.widget.__AbstractDataGridComposite;
import net.codecadenza.runtime.richclient.eclipse.widget.__DataGridComposite;
import net.codecadenza.runtime.richclient.format.FormatDTO;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Abstract generic base class for list-of-values dialogs
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the dialog
 */
public abstract class __AbstractLOVDialog<T> extends TitleAreaDialog {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static final String EMPTY_TITLE = "";

	private Text txtSearchInput;
	private String idValue;
	private String displayValue;
	private final String filter;
	protected boolean allowMultipleSelection;
	private __DataGridComposite<T> grid;
	private final boolean allowDeselection;
	protected FormatDTO userFormat;
	protected DecimalFormat decimalFormat;
	protected DateTimeFormatter dateTimeFormat;
	protected DateTimeFormatter dateFormat;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param filter
	 * @param allowMultipleSelection
	 * @param allowDeselection
	 */
	protected __AbstractLOVDialog(Shell parentShell, String filter, boolean allowMultipleSelection, boolean allowDeselection) {
		super(parentShell);

		this.filter = filter;
		this.allowMultipleSelection = allowMultipleSelection;
		this.allowDeselection = allowDeselection;
		this.userFormat = getFormatPreferences();
		this.decimalFormat = new DecimalFormat(userFormat.getDecimalFormat());
		this.dateTimeFormat = DateTimeFormatter.ofPattern(userFormat.getDateTimeFormat()).withZone(ZoneId.systemDefault());
		this.dateFormat = DateTimeFormatter.ofPattern(userFormat.getDateFormat()).withZone(ZoneId.systemDefault());
		this.setShellStyle(super.getShellStyle() | SWT.RESIZE);
	}

	/**
	 * Get the column text
	 * @param element
	 * @param columnIndex
	 * @return the column text
	 */
	public abstract String getColumnText(T element, int columnIndex);

	/**
	 * Fetch the data
	 * @param filter
	 * @return a list of data to be displayed
	 */
	public abstract List<T> fetchData(String filter);

	/**
	 * Set the ID value
	 * @param selectedElement
	 * @return the ID value
	 */
	public abstract String setIdValue(T selectedElement);

	/**
	 * Set the display value
	 * @param selectedElements
	 * @return the display value
	 */
	public abstract String setDisplayValue(List<T> selectedElements);

	/**
	 * Initialize the data grid
	 * @param grid
	 */
	public abstract void initColumnsOfGrid(__AbstractDataGridComposite<T> grid);

	/**
	 * @return the format preferences
	 */
	public abstract FormatDTO getFormatPreferences();

	/**
	 * Get the column image
	 * @param element
	 * @param columnIndex
	 * @return the image of a given column or null if no image should be displayed
	 */
	@SuppressWarnings("unused")
	public Image getColumnImage(T element, int columnIndex) {
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.TitleAreaDialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent);

		final var panMain = new Composite(panDialogArea, SWT.NONE);
		panMain.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		panMain.setLayout(new GridLayout(3, false));

		final var lblSearch = new Label(panMain, SWT.NONE);
		lblSearch.setText(getTranslationForFieldLabel(ABSTRACT_LOV_DIALOG_LBL_FILTER));

		final var gdSearchInput = new GridData(SWT.CENTER, SWT.CENTER, false, false);
		gdSearchInput.widthHint = 164;

		txtSearchInput = new Text(panMain, SWT.BORDER);
		txtSearchInput.setLayoutData(gdSearchInput);

		txtSearchInput.addKeyListener(new KeyAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.KeyAdapter#keyReleased(org.eclipse.swt.events.KeyEvent)
			 */
			@Override
			public void keyReleased(KeyEvent e) {
				doSearch(txtSearchInput.getText());
			}
		});

		txtSearchInput.setText(filter);
		txtSearchInput.selectAll();
		txtSearchInput.setFocus();

		final var cmdSearch = new Button(panMain, SWT.NONE);
		cmdSearch.setText(getTranslation(ABSTRACT_LOV_DIALOG_CMD_SEARCH));

		cmdSearch.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				doSearch(txtSearchInput.getText());
			}
		});

		int style = -1;

		if (allowMultipleSelection)
			style = SWT.BORDER | SWT.MULTI | SWT.FULL_SELECTION;
		else
			style = SWT.BORDER | SWT.SINGLE | SWT.FULL_SELECTION;

		grid = new __DataGridComposite<>(panMain, style) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.__AbstractDataGridComposite#getCellText(java.lang.Object, int)
			 */
			@Override
			public String getCellText(T element, int columnIndex) {
				return getColumnText(element, columnIndex);
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.__AbstractDataGridComposite#getCellImage(java.lang.Object, int)
			 */
			@Override
			public Image getCellImage(T element, int columnIndex) {
				return getColumnImage(element, columnIndex);
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.__AbstractDataGridComposite#onDoubleClick(java.lang.Object)
			 */
			@Override
			public void onDoubleClick(T element) {
				setReturnValues();

				if (idValue != null) {
					setReturnCode(Dialog.OK);
					close();
				}
			}
		};

		initColumnsOfGrid(grid);

		final var mniSelAll = new MenuItem(grid.getPopUpMenu(), SWT.NONE);
		mniSelAll.setText(getTranslation(ABSTRACT_LOV_DIALOG_CMD_SELECT_ALL));

		mniSelAll.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetDefaultSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				grid.getTableViewer().getTable().selectAll();
			}
		});

		final var mniDeselectAll = new MenuItem(grid.getPopUpMenu(), SWT.NONE);
		mniDeselectAll.setText(getTranslation(ABSTRACT_LOV_DIALOG_CMD_DESELECT_ALL));

		mniDeselectAll.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetDefaultSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				grid.getTableViewer().getTable().deselectAll();
			}
		});

		grid.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 3, 1));

		setTitle(getTranslation(ABSTRACT_LOV_DIALOG_MSG_TITLE));
		setTitleImage(ImageCache.getImage(ImageCache.IMG_TITLE_LOV));

		return panDialogArea;
	}

	/**
	 * Perform the search operation
	 * @param filter
	 */
	private void doSearch(String filter) {
		if (filter.isEmpty()) {
			grid.setData(Collections.emptyList());
			return;
		}

		final double startTime = System.currentTimeMillis();

		final Cursor defaultCursor = Display.getCurrent().getActiveShell().getCursor();
		Display.getCurrent().getActiveShell().setCursor(Display.getCurrent().getSystemCursor(SWT.CURSOR_WAIT));

		logger.debug("Search for items by using filter '{}'", filter);

		try {
			final List<T> result = fetchData(filter);

			// Measure the execution time
			final double endTime = System.currentTimeMillis();
			final double timeDif = endTime - startTime;
			final int recordCount = result.size();

			final var params = new ArrayList<>();
			params.add(recordCount);
			params.add(String.format("%.2f", timeDif / 1000));

			setMessage(getTranslation(DATA_FETCH_ACTION_RESULT_NO_COUNT, params.toArray()));

			// Set the input
			grid.setData(result);
		}
		catch (final Exception e) {
			setErrorMessage(getTranslation(DATA_FETCH_ACTION_MSG_QUERY_FAILED) + e.getMessage());

			logger.error("Error while fetching data!", e);
		}
		finally {
			Display.getCurrent().getActiveShell().setCursor(defaultCursor);
		}
	}

	/**
	 * Set the return values
	 */
	private void setReturnValues() {
		final T singleSel = grid.getSelection();
		final List<T> multiSel = grid.getAllSelectedElements();

		idValue = setIdValue(singleSel);
		displayValue = setDisplayValue(multiSel);
	}

	/**
	 * @return the ID value
	 */
	public String getIdValue() {
		return idValue;
	}

	/**
	 * @return the display value
	 */
	public String getDisplayValue() {
		return displayValue;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID) {
			setReturnValues();

			if (idValue == null)
				return;
		}
		else if (buttonId == IDialogConstants.DESELECT_ALL_ID) {
			idValue = null;
			displayValue = null;
			setReturnCode(Dialog.OK);
			close();
			return;
		}

		super.buttonPressed(buttonId);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		createButton(parent, IDialogConstants.OK_ID, getTranslation(CMD_OK), false);

		if (allowDeselection)
			createButton(parent, IDialogConstants.DESELECT_ALL_ID, getTranslation(ABSTRACT_LOV_DIALOG_CMD_DESELECT), false);

		createButton(parent, IDialogConstants.CANCEL_ID, getTranslation(CMD_CANCEL), false);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
	 */
	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);
		newShell.setText(getTitle());
	}

	/**
	 * Get the title of this list-of-values dialog. This method is intended to be overwritten!
	 * @return the dialog title
	 */
	public String getTitle() {
		return EMPTY_TITLE;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#close()
	 */
	@Override
	public boolean close() {
		grid.dispose();

		return super.close();
	}

}
