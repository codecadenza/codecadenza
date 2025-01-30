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
package net.codecadenza.eclipse.tools.reverse.dialog;

import java.util.ArrayList;
import net.codecadenza.eclipse.model.db.DBColumn;
import net.codecadenza.eclipse.model.db.PrimaryKey;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaDialog;
import net.codecadenza.eclipse.tools.reverse.model.RevEngDomainObject;
import net.codecadenza.eclipse.tools.reverse.model.ReverseEngineeringModel;
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
 * Dialog for selecting a target domain object
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class AssociationTargetDialog extends CodeCadenzaDialog {
	private static final String DLG_TITLE = "Select target domain object";

	private final ReverseEngineeringModel revEngModel;
	private DataComboViewer<DomainObject> cboTarget;
	private DomainObject selectedDomainObject;
	private final DomainAttribute domainAttribute;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param domainAttribute
	 * @param revEngModel
	 */
	public AssociationTargetDialog(Shell parentShell, DomainAttribute domainAttribute, ReverseEngineeringModel revEngModel) {
		super(parentShell);

		this.revEngModel = revEngModel;
		this.domainAttribute = domainAttribute;
	}

	/**
	 * @return the selected domain object
	 */
	public DomainObject getSelectedDomainObject() {
		return selectedDomainObject;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent, 2, false);

		final var lblDomainObj = new Label(panDialogArea, SWT.NONE);
		lblDomainObj.setText("Target domain object:");

		final var gdTarget = new GridData(SWT.FILL, SWT.CENTER, true, false);
		gdTarget.widthHint = 300;

		cboTarget = new DataComboViewer<>(panDialogArea, SWT.BORDER | SWT.READ_ONLY) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.DataComboViewer#getItemText(java.lang.Object)
			 */
			@Override
			public String getItemText(DomainObject type) {
				return type.getName();
			}
		};

		cboTarget.setLayoutData(gdTarget);

		final var targetList = new ArrayList<DomainObject>();
		final String attrColTypeName = domainAttribute.getColumn().getColumnType().getName();

		// Search for valid target domain objects
		for (final RevEngDomainObject revEngObj : revEngModel.getDomainObjects()) {
			final DomainObject domainObject = revEngObj.getDomainObject();

			if (domainObject.isMappedSuperClass())
				continue;

			if (domainObject.getDatabaseTable() == null)
				continue;

			final PrimaryKey primaryKey = domainObject.getDatabaseTable().getPrimaryKey();

			if (primaryKey == null)
				continue;

			for (final DBColumn col : domainObject.getDatabaseTable().getColumns()) {
				if (!col.equals(primaryKey.getColumn()))
					continue;

				// A domain object can only be a valid target if the types of the primary key and the attribute column match!
				if (col.getColumnType().getName().equalsIgnoreCase(attrColTypeName))
					targetList.add(domainObject);
			}
		}

		cboTarget.setData(targetList);

		if (!targetList.isEmpty())
			cboTarget.setSelectedItem(targetList.get(0));

		return panDialogArea;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID) {
			selectedDomainObject = cboTarget.getSelectedItem();

			if (selectedDomainObject == null) {
				MessageDialog.openInformation(getShell(), DLG_TITLE, "A target domain object must be selected!");
				return;
			}
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
