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
package net.codecadenza.eclipse.diagram.domain.dialog;

import static net.codecadenza.eclipse.shared.Constants.DB_TABLE_SUFFIX;

import net.codecadenza.eclipse.model.db.DBColumn;
import net.codecadenza.eclipse.model.db.DBTable;
import net.codecadenza.eclipse.model.db.Database;
import net.codecadenza.eclipse.model.domain.AssociationTagEnumeration;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaDialog;
import net.codecadenza.eclipse.tools.dbsync.util.DBObjectValidationException;
import net.codecadenza.eclipse.tools.dbsync.util.DBSynchService;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Dialog for creating and maintaining many-to-many associations
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditManyToManyAssociationDialog extends CodeCadenzaDialog {
	private static final String DLG_TITLE_EDIT = "Edit many-to-many association";
	private static final String DLG_TITLE_NEW = "Create new many-to-many association";

	private Label lblReverseAssocName;
	private Text txtReverseAssocName;
	private Text txtAssociationName;
	private Button chkFetchTypeEager;
	private Button chkBidirectional;
	private Button chkAddUniqueKey;
	private final DomainObject target;
	private final ManyToManyAssociation association;
	private Combo cboTag;
	private final boolean doEdit;
	private final DomainObject source;
	private Text txtJoinTableName;
	private Text txtJoinColumnName1;
	private Text txtJoinColumnName2;
	private String joinTableName;
	private String joinColumnName1;
	private String joinColumnName2;
	private boolean addUniqueKey;
	private String reverseAssociationName;
	private Text txtInternalComment;
	private Text txtUserComment;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param doEdit
	 * @param association
	 */
	public EditManyToManyAssociationDialog(Shell parentShell, boolean doEdit, ManyToManyAssociation association) {
		super(parentShell);

		this.doEdit = doEdit;
		this.source = association.getDomainObject();
		this.target = association.getTarget();
		this.association = association;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent, 2);

		final var lblAssociationName = new Label(panDialogArea, SWT.NONE);
		lblAssociationName.setText("Name:");

		final var gdAssociationName = new GridData(SWT.FILL, SWT.CENTER, true, false);
		gdAssociationName.widthHint = 300;

		txtAssociationName = new Text(panDialogArea, SWT.BORDER);
		txtAssociationName.setLayoutData(gdAssociationName);
		txtAssociationName.setEnabled(false);

		final var lblFetchTypeEager = new Label(panDialogArea, SWT.NONE);
		lblFetchTypeEager.setText("Fetch type eager:");

		chkFetchTypeEager = new Button(panDialogArea, SWT.CHECK);

		if (!doEdit || association.isOwner()) {
			final var lblJoinTable = new Label(panDialogArea, SWT.NONE);
			lblJoinTable.setText("Join table:");

			txtJoinTableName = new Text(panDialogArea, SWT.BORDER);
			txtJoinTableName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
			txtJoinTableName.setEnabled(false);

			final var lblJoinCol1 = new Label(panDialogArea, SWT.NONE);
			lblJoinCol1.setText("Join column 1:");

			txtJoinColumnName1 = new Text(panDialogArea, SWT.BORDER);
			txtJoinColumnName1.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
			txtJoinColumnName1.setEnabled(false);

			final var lblJoinCol2 = new Label(panDialogArea, SWT.NONE);
			lblJoinCol2.setText("Join column 2:");

			txtJoinColumnName2 = new Text(panDialogArea, SWT.BORDER);
			txtJoinColumnName2.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
			txtJoinColumnName2.setEnabled(false);
		}

		if (!doEdit) {
			txtJoinTableName.setEnabled(true);
			txtJoinColumnName1.setEnabled(true);
			txtJoinColumnName2.setEnabled(true);
			txtAssociationName.setEnabled(true);

			final String assocName = target.getNamePlural().substring(0, 1).toLowerCase() + target.getNamePlural().substring(1);
			txtAssociationName.setText(assocName);

			DBTable sourceTable = source.getDatabaseTable();

			if (sourceTable == null)
				sourceTable = source.getRootParentDomainObject(false).getDatabaseTable();

			DBTable targetTable = target.getDatabaseTable();

			if (targetTable == null)
				targetTable = target.getRootParentDomainObject(false).getDatabaseTable();

			final String nameSuffix = EclipseIDEService.buildDefaultColumnName(assocName);

			txtJoinTableName.setText(sourceTable.getShortTableName() + "_" + nameSuffix + DB_TABLE_SUFFIX);
			txtJoinColumnName1.setText(sourceTable.getShortTableName() + "_pk");
			txtJoinColumnName2.setText(targetTable.getShortTableName() + "_pk");

			if (txtJoinColumnName1.getText().equals(txtJoinColumnName2.getText()))
				txtJoinColumnName2.setText(targetTable.getShortTableName() + "2_pk");

			// Tagging can be defined only when an association is created!
			final var lblTag = new Label(panDialogArea, SWT.NONE);
			lblTag.setText("Tag:");

			cboTag = new Combo(panDialogArea, SWT.READ_ONLY);
			cboTag.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

			source.getValidAssociationTags(null).forEach(cboTag::add);

			cboTag.select(0);
		}
		else {
			txtAssociationName.setText(association.getName());
			chkFetchTypeEager.setSelection(association.isFetchTypeEager());
		}

		if (!doEdit) {
			final var lblAddUniqueKey = new Label(panDialogArea, SWT.NONE);
			lblAddUniqueKey.setText("Add unique key:");

			chkAddUniqueKey = new Button(panDialogArea, SWT.CHECK);
			chkAddUniqueKey.setSelection(true);
		}

		final var lblBidirectional = new Label(panDialogArea, SWT.NONE);
		lblBidirectional.setText("Bidirectional:");

		chkBidirectional = new Button(panDialogArea, SWT.CHECK);
		chkBidirectional.setEnabled(false);

		chkBidirectional.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (chkBidirectional.getSelection()) {
					final String revProp = source.getNamePlural().substring(0, 1).toLowerCase() + source.getNamePlural().substring(1);
					txtReverseAssocName.setText(revProp);
					txtReverseAssocName.setEnabled(true);
					lblReverseAssocName.setEnabled(true);
				}
				else {
					txtReverseAssocName.setEnabled(false);
					lblReverseAssocName.setEnabled(false);
				}
			}
		});

		lblReverseAssocName = new Label(panDialogArea, SWT.NONE);
		lblReverseAssocName.setText("Reverse name:");
		lblReverseAssocName.setEnabled(false);

		txtReverseAssocName = new Text(panDialogArea, SWT.BORDER);
		txtReverseAssocName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtReverseAssocName.setEnabled(false);

		final var lblUserComment = new Label(panDialogArea, SWT.NONE);
		lblUserComment.setText("User comment:");

		final var gdUserComment = new GridData(SWT.FILL, SWT.FILL, true, true, 2, 1);
		gdUserComment.heightHint = 50;

		txtUserComment = new Text(panDialogArea, SWT.V_SCROLL | SWT.MULTI | SWT.BORDER | SWT.H_SCROLL);
		txtUserComment.setLayoutData(gdUserComment);

		final var lblInternalComment = new Label(panDialogArea, SWT.NONE);
		lblInternalComment.setText("Internal comment:");

		final var gdInternalComment = new GridData(SWT.FILL, SWT.FILL, true, true, 2, 1);
		gdInternalComment.heightHint = 50;

		txtInternalComment = new Text(panDialogArea, SWT.V_SCROLL | SWT.MULTI | SWT.BORDER | SWT.H_SCROLL);
		txtInternalComment.setLayoutData(gdInternalComment);

		if (!doEdit) {
			chkBidirectional.setSelection(false);
			chkBidirectional.setEnabled(true);
			txtReverseAssocName.setEnabled(true);
		}
		else {
			if (association.isOwner()) {
				final DBTable assocTable = association.getTable();
				boolean firstColumn = true;

				txtJoinTableName.setText(assocTable.getName());

				for (final DBColumn col : assocTable.getColumns()) {
					if (assocTable.getPrimaryKey() != null && col.equals(assocTable.getPrimaryKey().getColumn()))
						continue;

					if (firstColumn) {
						firstColumn = false;
						txtJoinColumnName1.setText(col.getName());
					}
					else
						txtJoinColumnName2.setText(col.getName());
				}
			}

			if (association.isBidirectional()) {
				chkBidirectional.setSelection(true);
				txtReverseAssocName.setText(association.getReverseAssociation().getName());
				txtReverseAssocName.setEnabled(true);
				lblReverseAssocName.setEnabled(true);
			}

			txtInternalComment.setText(association.getInternalComment() == null ? "" : association.getInternalComment());
			txtUserComment.setText(association.getUserComment() == null ? "" : association.getUserComment());
		}

		return panDialogArea;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID && !writeInputsToObject())
			return;

		super.buttonPressed(buttonId);
	}

	/**
	 * Apply the input
	 * @return true if the validation was successful
	 */
	private boolean writeInputsToObject() {
		if (!doEdit) {
			final Database db = source.getNamespace().getProject().getDatabase();

			IStatus status = EclipseIDEService.validateFieldName(txtAssociationName.getText());

			if (status.getSeverity() > IStatus.INFO) {
				MessageDialog.openInformation(getShell(), DLG_TITLE_NEW, status.getMessage());
				txtAssociationName.setFocus();
				return false;
			}

			if (chkBidirectional.getSelection()) {
				status = EclipseIDEService.validateFieldName(txtReverseAssocName.getText());

				if (status.getSeverity() > IStatus.INFO) {
					MessageDialog.openInformation(getShell(), DLG_TITLE_NEW, status.getMessage());
					txtReverseAssocName.setFocus();
					return false;
				}

				reverseAssociationName = txtReverseAssocName.getText();
			}
			else
				reverseAssociationName = null;

			joinTableName = txtJoinTableName.getText();
			joinColumnName1 = txtJoinColumnName1.getText();
			joinColumnName2 = txtJoinColumnName2.getText();

			try {
				new DBSynchService(db).validateIdentifier(joinColumnName1);
			}
			catch (final DBObjectValidationException e) {
				MessageDialog.openInformation(getShell(), DLG_TITLE_NEW, e.getMessage());
				txtJoinColumnName1.setFocus();
				return false;
			}

			try {
				new DBSynchService(db).validateIdentifier(joinColumnName2);
			}
			catch (final DBObjectValidationException e) {
				MessageDialog.openInformation(getShell(), DLG_TITLE_NEW, e.getMessage());
				txtJoinColumnName2.setFocus();
				return false;
			}

			try {
				final DBTable table = source.getRootParentDomainObject(false).getDatabaseTable();
				new DBSynchService(db).validateTableName(table.getSchemaName(), table.getCatalogName(), joinTableName);
			}
			catch (final DBObjectValidationException e) {
				MessageDialog.openInformation(getShell(), DLG_TITLE_NEW, e.getMessage());
				txtJoinTableName.setFocus();
				return false;
			}

			if (joinColumnName1.equals(joinColumnName2)) {
				MessageDialog.openInformation(getShell(), DLG_TITLE_NEW, "The names of the join columns must be different!");
				txtJoinColumnName1.setFocus();
				return false;
			}

			addUniqueKey = chkAddUniqueKey.getSelection();

			association.setTag(AssociationTagEnumeration.valueOf(cboTag.getItem(cboTag.getSelectionIndex())));
			association.setName(txtAssociationName.getText());
			association.setOwner(true);
		}

		association.setFetchTypeEager(chkFetchTypeEager.getSelection());
		association.setInternalComment(txtInternalComment.getText());
		association.setUserComment(txtUserComment.getText());

		return true;
	}

	/**
	 * @return the name of the join table
	 */
	public String getJoinTableName() {
		return joinTableName;
	}

	/**
	 * @return the name of the first join column
	 */
	public String getJoinColumnName1() {
		return joinColumnName1;
	}

	/**
	 * @return the name of the second join column
	 */
	public String getJoinColumnName2() {
		return joinColumnName2;
	}

	/**
	 * @return true if a unique key should be added
	 */
	public boolean isAddUniqueKey() {
		return addUniqueKey;
	}

	/**
	 * @return the name of the reverse association
	 */
	public String getReverseAssociationName() {
		return reverseAssociationName;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
	 */
	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);

		if (doEdit)
			newShell.setText(DLG_TITLE_EDIT);
		else
			newShell.setText(DLG_TITLE_NEW);
	}

	/**
	 * @return the association
	 */
	public ManyToManyAssociation getAssociation() {
		return association;
	}

}
