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

import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.CMD_EXPORT;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.COPY_CELL_TO_CLIPBOARD_ACTION_NAME;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.COPY_COLUMN_TO_CLIPBOARD_ACTION_NAME;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.COPY_ROW_TO_CLIPBOARD_ACTION_NAME;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.getTranslation;

import net.codecadenza.runtime.richclient.eclipse.action.ExportXLSXAction;
import net.codecadenza.runtime.richclient.eclipse.image.ImageCache;
import net.codecadenza.runtime.richclient.eclipse.widget.util.TableViewerClipboardHelper;
import net.codecadenza.runtime.richclient.format.FormatDTO;
import net.codecadenza.runtime.richclient.format.FormatPreferencesManager;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.ToolItem;

/**
 * <p>
 * Abstract base class for displaying resultsets in a grid
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the data grid
 */
public abstract class AbstractGridResultPanel<T> extends __AbstractGridResultPanel<T> {
	protected TableViewerClipboardHelper clipboardHelper;

	/**
	 * Constructor
	 * @param parent
	 * @param style
	 */
	protected AbstractGridResultPanel(final Composite parent, int style) {
		super(parent, style);

		final var cmdExport = new ToolItem(toolBar, SWT.PUSH);
		cmdExport.setImage(ImageCache.getImage(ImageCache.IMG_EXCEL));
		cmdExport.setToolTipText(getTranslation(CMD_EXPORT));

		cmdExport.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				new ExportXLSXAction(Display.getCurrent().getActiveShell(), AbstractGridResultPanel.this).run();
			}
		});

		clipboardHelper = new TableViewerClipboardHelper(parent.getDisplay(), tableViewer);

		table.addKeyListener(new KeyAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.KeyAdapter#keyReleased(org.eclipse.swt.events.KeyEvent)
			 */
			@Override
			public void keyReleased(KeyEvent e) {
				if (e.stateMask == SWT.CTRL && e.keyCode != SWT.CTRL && e.keyCode == 99)
					clipboardHelper.copySelectionToClipboard();
			}
		});

		final var mniCopy = new MenuItem(popUpMenu, SWT.NONE);
		mniCopy.setText(getTranslation(COPY_CELL_TO_CLIPBOARD_ACTION_NAME));

		mniCopy.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(final SelectionEvent e) {
				clipboardHelper.copySelectionToClipboard();
			}
		});

		final var mniCopyRow = new MenuItem(popUpMenu, SWT.NONE);
		mniCopyRow.setText(getTranslation(COPY_ROW_TO_CLIPBOARD_ACTION_NAME));

		mniCopyRow.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				clipboardHelper.copyRowContentToClipboard();
			}
		});

		final var mniCopyCol = new MenuItem(popUpMenu, SWT.NONE);
		mniCopyCol.setText(getTranslation(COPY_COLUMN_TO_CLIPBOARD_ACTION_NAME));

		mniCopyCol.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				clipboardHelper.copyColumnContentToClipboard();
			}
		});
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.eclipse.search.__AbstractGridResultPanel#getFormatPreferences()
	 */
	@Override
	public FormatDTO getFormatPreferences() {
		return FormatPreferencesManager.getFormatDTO();
	}

}
