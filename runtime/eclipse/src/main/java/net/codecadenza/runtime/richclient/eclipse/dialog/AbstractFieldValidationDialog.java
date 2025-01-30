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
package net.codecadenza.runtime.richclient.eclipse.dialog;

import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.CMD_OK;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.FIELD_VALIDATION_DIALOG_COL_DESC;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.FIELD_VALIDATION_DIALOG_COL_NAME;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.FIELD_VALIDATION_DIALOG_TITLE;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.FIELD_VALIDATION_DIALOG_TITLE_MSG;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.FIELD_VALIDATION_DIALOG_TITLE_MSG_DETAIL;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.getTranslation;

import java.util.Collection;
import net.codecadenza.runtime.richclient.eclipse.image.ImageCache;
import net.codecadenza.runtime.richclient.eclipse.widget.ColumnSortType;
import net.codecadenza.runtime.richclient.eclipse.widget.__DataGridComposite;
import net.codecadenza.runtime.richclient.validation.FieldValidationBean;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

/**
 * <p>
 * Abstract base class for field validation error dialogs
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractFieldValidationDialog extends TitleAreaDialog {
	private final Collection<FieldValidationBean> fieldValidationObjects;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param fieldValidationObjects
	 */
	protected AbstractFieldValidationDialog(Shell parentShell, Collection<FieldValidationBean> fieldValidationObjects) {
		super(parentShell);

		this.fieldValidationObjects = fieldValidationObjects;
		this.setShellStyle(super.getShellStyle() | SWT.RESIZE);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent);

		final var panMain = new Composite(panDialogArea, SWT.NONE);
		panMain.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		panMain.setLayout(new GridLayout());

		final __DataGridComposite<FieldValidationBean> grid = new __DataGridComposite<>(panMain, SWT.BORDER) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.__AbstractDataGridComposite#getCellText(java.lang.Object, int)
			 */
			@Override
			public String getCellText(FieldValidationBean element, int columnIndex) {
				if (columnIndex == 0)
					return element.getFieldLabel();

				return element.getErrorText();
			}
		};

		grid.addColumn(getTranslation(FIELD_VALIDATION_DIALOG_COL_NAME), ColumnSortType.STRING, null, 150);
		grid.addColumn(getTranslation(FIELD_VALIDATION_DIALOG_COL_DESC), ColumnSortType.STRING, null, 250);
		grid.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		grid.setData(fieldValidationObjects);

		setTitle(getTranslation(FIELD_VALIDATION_DIALOG_TITLE_MSG));
		setMessage(getTranslation(FIELD_VALIDATION_DIALOG_TITLE_MSG_DETAIL));
		setTitleImage(ImageCache.getImage("validation_error.png"));

		return panDialogArea;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		createButton(parent, IDialogConstants.OK_ID, getTranslation(CMD_OK), true);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
	 */
	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);
		newShell.setText(getTranslation(FIELD_VALIDATION_DIALOG_TITLE));
		newShell.setImage(ImageCache.getImage(ImageCache.IMG_ERROR));
	}

}
