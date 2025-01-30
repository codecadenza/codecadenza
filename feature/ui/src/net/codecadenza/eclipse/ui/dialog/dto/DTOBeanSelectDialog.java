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
package net.codecadenza.eclipse.ui.dialog.dto;

import java.util.ArrayList;
import java.util.List;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaDialog;
import net.codecadenza.runtime.richclient.eclipse.widget.DataComboViewer;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

/**
 * <p>
 * Dialog for selecting an existing DTO
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DTOBeanSelectDialog extends CodeCadenzaDialog {
	private static final String DLG_TITLE = "Select DTO";

	private final DomainObject domainObject;
	private DataComboViewer<DTOBean> cboDTOBean;
	private DTOBean dtoBean;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param domainObject
	 */
	public DTOBeanSelectDialog(Shell parentShell, DomainObject domainObject) {
		super(parentShell);

		this.domainObject = domainObject;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent, 2, false);

		final var lblDTOBean = new Label(panDialogArea, SWT.NONE);
		lblDTOBean.setText("DTO:");

		final var gdDTOBean = new GridData(SWT.FILL, SWT.CENTER, true, false);
		gdDTOBean.widthHint = 300;

		cboDTOBean = new DataComboViewer<>(panDialogArea, SWT.BORDER | SWT.READ_ONLY) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.DataComboViewer#getItemText(java.lang.Object)
			 */
			@Override
			public String getItemText(DTOBean type) {
				return type.getName();
			}
		};

		// Search for all suitable data transfer objects and sort all elements
		final List<DTOBean> dtoList = new ArrayList<>(domainObject.getNamespace().getProject().getDTOsOfDomainObject(domainObject));
		final List<DTOBean> sortedList = dtoList.stream().filter(e -> !e.isVirtual())
				.sorted((a, b) -> a.getName().compareToIgnoreCase(b.getName())).toList();

		cboDTOBean.setLayoutData(gdDTOBean);
		cboDTOBean.setData(sortedList);

		if (!dtoList.isEmpty())
			cboDTOBean.setSelectedItem(sortedList.get(0));

		return panDialogArea;
	}

	/**
	 * @return the selected data transfer object
	 */
	public DTOBean getDtoBean() {
		return dtoBean;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID) {
			if (cboDTOBean.getSelectedItem() == null) {
				MessageDialog.openInformation(getShell(), DLG_TITLE, "A DTO must be selected!");
				return;
			}

			dtoBean = cboDTOBean.getSelectedItem();
		}

		super.buttonPressed(buttonId);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
	 */
	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);

		newShell.setText(DLG_TITLE);
	}

}
