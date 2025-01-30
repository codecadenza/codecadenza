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
package net.codecadenza.eclipse.ui.preview.util;

import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.FormTable;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.client.TableColumnField;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.ui.dialog.client.EditTableColumnFieldDialog;
import net.codecadenza.eclipse.ui.preview.event.PreviewChangeController;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TableColumn;

/**
 * <p>
 * Listener for detecting that a table column has been selected
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class TableColumnSelectionListener implements Listener {
	private final Shell shell = Display.getCurrent().getActiveShell();
	private final Project project;
	private final FormTypeEnumeration formType;
	private final PreviewChangeController previewChangeController;
	private final FormTable formTable;

	/**
	 * Constructor
	 * @param panel
	 * @param previewChangeController
	 */
	public TableColumnSelectionListener(FormPanel panel, PreviewChangeController previewChangeController) {
		this.project = panel.getAssociation().getDomainObject().getNamespace().getProject();
		this.formType = FormTypeEnumeration.GRID;
		this.formTable = panel.getFormTable();
		this.previewChangeController = previewChangeController;
	}

	/**
	 * Constructor
	 * @param form
	 * @param previewChangeController
	 */
	public TableColumnSelectionListener(Form form, PreviewChangeController previewChangeController) {
		this.project = form.getDomainObject().getNamespace().getProject();
		this.formType = form.getFormType();
		this.formTable = form.getFormPanels().get(0).getFormTable();
		this.previewChangeController = previewChangeController;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.swt.widgets.Listener#handleEvent(org.eclipse.swt.widgets.Event)
	 */
	@Override
	public void handleEvent(Event event) {
		final var tableColumn = (TableColumn) event.widget;

		for (final TableColumnField column : formTable.getFields()) {
			if (!column.getTitle().equals(tableColumn.getText()))
				continue;

			final var dlg = new EditTableColumnFieldDialog(shell, project, column, formType);

			if (dlg.open() != Dialog.CANCEL) {
				previewChangeController.fireChangeEvent();
				return;
			}
		}
	}

}
