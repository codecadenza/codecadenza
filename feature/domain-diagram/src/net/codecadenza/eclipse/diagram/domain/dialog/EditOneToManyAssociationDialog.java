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

import java.util.Objects;
import net.codecadenza.eclipse.model.db.DBColumn;
import net.codecadenza.eclipse.model.db.DBTable;
import net.codecadenza.eclipse.model.db.Database;
import net.codecadenza.eclipse.model.domain.AssociationTagEnumeration;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
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
import org.eclipse.swt.graphics.Point;
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
 * Dialog for creating and maintaining one-to-many associations
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditOneToManyAssociationDialog extends CodeCadenzaDialog {
	private static final String DLG_TITLE_EDIT = "Edit one-to-many association";
	private static final String DLG_TITLE_NEW = "Create new one-to-many association";

	private Text txtAssociationName;
	private Button chkCascadePersist;
	private Button chkCascadeMerge;
	private Button chkCascadeRefresh;
	private Button chkCascadeRemove;
	private Button chkFetchTypeEager;
	private final DomainObject target;
	private final OneToManyAssociation association;
	private Text txtReversePropertyName;
	private final boolean doEdit;
	private Button chkBidirectional;
	private final DomainObject source;
	private Combo cboTag;
	private Text txtJoinTableName;
	private Text txtJoinColumnName1;
	private Text txtJoinColumnName2;
	private String joinTableName;
	private String joinColumnName1;
	private String joinColumnName2;
	private String reverseAssociationName;
	private Text txtInternalComment;
	private Text txtUserComment;
	private Button chkAddUniqueKey;
	private boolean addUniqueKey;
	private final DBTable sourceTable;
	private final DBTable targetTable;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param doEdit
	 * @param source
	 * @param target
	 * @param association
	 */
	public EditOneToManyAssociationDialog(Shell parentShell, boolean doEdit, DomainObject source, DomainObject target,
			OneToManyAssociation association) {
		super(parentShell);

		this.doEdit = doEdit;
		this.source = source;
		this.target = target;
		this.association = association;
		this.sourceTable = Objects.requireNonNullElseGet(source.getDatabaseTable(),
				() -> source.getRootParentDomainObject(false).getDatabaseTable());
		this.targetTable = Objects.requireNonNullElseGet(target.getDatabaseTable(),
				() -> target.getRootParentDomainObject(false).getDatabaseTable());
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent, 4);

		final var lblAssociationName = new Label(panDialogArea, SWT.NONE);
		lblAssociationName.setText("Name:");

		txtAssociationName = new Text(panDialogArea, SWT.BORDER);
		txtAssociationName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblBidirectional = new Label(panDialogArea, SWT.NONE);
		lblBidirectional.setText("Bidirectional:");

		chkBidirectional = new Button(panDialogArea, SWT.CHECK);

		chkBidirectional.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (doEdit)
					return;

				txtJoinTableName.setEnabled(!chkBidirectional.getSelection());
				txtJoinTableName.setText("");
				txtJoinColumnName2.setEnabled(!chkBidirectional.getSelection());
				txtJoinColumnName2.setText("");
				txtReversePropertyName.setEnabled(chkBidirectional.getSelection());
				txtReversePropertyName.setText("");
				chkAddUniqueKey.setEnabled(!chkBidirectional.getSelection());
				chkCascadeMerge.setSelection(chkBidirectional.getSelection());
				chkCascadePersist.setSelection(chkBidirectional.getSelection());
				chkCascadeRemove.setSelection(chkBidirectional.getSelection());

				if (!chkBidirectional.getSelection()) {
					final String assocName = target.getNamePlural().substring(0, 1).toLowerCase() + target.getNamePlural().substring(1);
					final String nameSuffix = EclipseIDEService.buildDefaultColumnName(assocName);

					txtJoinTableName.setText(sourceTable.getShortTableName() + "_" + nameSuffix + DB_TABLE_SUFFIX);
					txtJoinColumnName1.setText(sourceTable.getShortTableName() + "_pk");
					txtJoinColumnName2.setText(targetTable.getShortTableName() + "_pk");

					if (txtJoinColumnName1.getText().equals(txtJoinColumnName2.getText()))
						txtJoinColumnName2.setText(targetTable.getShortTableName() + "2_pk");
				}
				else {
					txtReversePropertyName.setText(createReversePropertyName());
					txtJoinColumnName1.setText(EclipseIDEService.buildDefaultColumnName(createReversePropertyName()));
				}
			}
		});

		final var lblFetchTypeEager = new Label(panDialogArea, SWT.NONE);
		lblFetchTypeEager.setText("Fetch type eager:");

		chkFetchTypeEager = new Button(panDialogArea, SWT.CHECK);

		final var lblAddUniqueKey = new Label(panDialogArea, SWT.NONE);
		lblAddUniqueKey.setText("Add unique key:");

		chkAddUniqueKey = new Button(panDialogArea, SWT.CHECK);
		chkAddUniqueKey.setSelection(true);
		chkAddUniqueKey.setEnabled(false);

		final var lblCascadePersist = new Label(panDialogArea, SWT.NONE);
		lblCascadePersist.setText("Cascade persist:");

		chkCascadePersist = new Button(panDialogArea, SWT.CHECK);

		final var lblCascadeMerge = new Label(panDialogArea, SWT.NONE);
		lblCascadeMerge.setText("Cascade merge:");

		chkCascadeMerge = new Button(panDialogArea, SWT.CHECK);

		final var lblCascadeRefresh = new Label(panDialogArea, SWT.NONE);
		lblCascadeRefresh.setText("Cascade refresh:");

		chkCascadeRefresh = new Button(panDialogArea, SWT.CHECK);

		final var lblCascadeRemove = new Label(panDialogArea, SWT.NONE);
		lblCascadeRemove.setText("Cascade remove:");

		chkCascadeRemove = new Button(panDialogArea, SWT.CHECK);

		if (!doEdit) {
			// Tagging can be defined only when creating the association!
			final var lblTag = new Label(panDialogArea, SWT.NONE);
			lblTag.setText("Tag:");

			cboTag = new Combo(panDialogArea, SWT.READ_ONLY);
			cboTag.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

			source.getValidAssociationTags(null).forEach(cboTag::add);

			cboTag.select(0);
		}

		final var lblReverseAssocName = new Label(panDialogArea, SWT.NONE);
		lblReverseAssocName.setText("Reverse name:");

		txtReversePropertyName = new Text(panDialogArea, SWT.BORDER);
		txtReversePropertyName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtReversePropertyName.setEnabled(false);

		final var lblJoinTableName = new Label(panDialogArea, SWT.NONE);
		lblJoinTableName.setText("Join table:");

		txtJoinTableName = new Text(panDialogArea, SWT.BORDER);
		txtJoinTableName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtJoinTableName.setEnabled(false);

		if (!doEdit) {
			new Label(panDialogArea, SWT.NONE);
			new Label(panDialogArea, SWT.NONE);
		}

		final Label lblJoinColumnName1 = new Label(panDialogArea, SWT.NONE);
		lblJoinColumnName1.setText("Join column name 1:");

		txtJoinColumnName1 = new Text(panDialogArea, SWT.BORDER);
		txtJoinColumnName1.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblJoinColumnName2 = new Label(panDialogArea, SWT.NONE);
		lblJoinColumnName2.setText("Join column name 2:");

		txtJoinColumnName2 = new Text(panDialogArea, SWT.BORDER);
		txtJoinColumnName2.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtJoinColumnName2.setEnabled(false);

		final var lblUserComment = new Label(panDialogArea, SWT.NONE);
		lblUserComment.setText("User comment:");

		final var gdUserComment = new GridData(SWT.FILL, SWT.FILL, true, true, 3, 1);
		gdUserComment.heightHint = 50;

		txtUserComment = new Text(panDialogArea, SWT.V_SCROLL | SWT.MULTI | SWT.BORDER | SWT.H_SCROLL);
		txtUserComment.setLayoutData(gdUserComment);

		final var lblInternalComment = new Label(panDialogArea, SWT.NONE);
		lblInternalComment.setText("Internal comment:");

		final var gdInternalComment = new GridData(SWT.FILL, SWT.FILL, true, true, 3, 1);
		gdInternalComment.heightHint = 50;

		txtInternalComment = new Text(panDialogArea, SWT.V_SCROLL | SWT.MULTI | SWT.BORDER | SWT.H_SCROLL);
		txtInternalComment.setLayoutData(gdInternalComment);

		if (!doEdit) {
			txtReversePropertyName.setEnabled(true);
			txtAssociationName.setText(createDefaultAssociationName());
			txtReversePropertyName.setText(createReversePropertyName());
			txtJoinColumnName1.setText(EclipseIDEService.buildDefaultColumnName(createReversePropertyName()));
			chkCascadeMerge.setSelection(true);
			chkCascadePersist.setSelection(true);
			chkCascadeRefresh.setSelection(true);
			chkCascadeRemove.setSelection(true);
			chkBidirectional.setSelection(true);
		}
		else {
			txtAssociationName.setText(association.getName());
			txtJoinColumnName1.setEnabled(false);
			chkBidirectional.setSelection(association.isBidirectional());
			chkBidirectional.setEnabled(false);
			chkCascadeMerge.setSelection(association.isCascadeMerge());
			chkCascadePersist.setSelection(association.isCascadePersist());
			chkCascadeRefresh.setSelection(association.isCascadeRefresh());
			chkCascadeRemove.setSelection(association.isCascadeRemove());
			chkFetchTypeEager.setSelection(association.isFetchTypeEager());
			txtInternalComment.setText(association.getInternalComment() == null ? "" : association.getInternalComment());
			txtUserComment.setText(association.getUserComment() == null ? "" : association.getUserComment());

			if (association.isBidirectional()) {
				txtReversePropertyName.setText(association.getReverseAssociation().getName());
				txtJoinColumnName1.setText(association.getReverseAssociation().getColumn().getName());
			}
			else {
				final DBTable assocTable = association.getTable();
				boolean firstColumn = true;

				txtJoinTableName.setText(assocTable.getName());

				for (final DBColumn col : association.getTable().getColumns()) {
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
		}

		return panDialogArea;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#getInitialSize()
	 */
	@Override
	protected Point getInitialSize() {
		return new Point(Math.max(600, super.getInitialSize().x), super.getInitialSize().y);
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
			final Database db = target.getNamespace().getProject().getDatabase();
			joinTableName = txtJoinTableName.getText();
			joinColumnName1 = txtJoinColumnName1.getText();
			joinColumnName2 = txtJoinColumnName2.getText();
			addUniqueKey = chkAddUniqueKey.getSelection();

			IStatus status = EclipseIDEService.validateFieldName(txtAssociationName.getText());

			if (status.getSeverity() > IStatus.INFO) {
				MessageDialog.openInformation(getShell(), DLG_TITLE_NEW, status.getMessage());
				txtAssociationName.setFocus();
				return false;
			}

			try {
				new DBSynchService(db).validateColumnName(targetTable, txtJoinColumnName1.getText());
			}
			catch (final DBObjectValidationException ex) {
				MessageDialog.openInformation(getShell(), DLG_TITLE_NEW, ex.getMessage());
				txtJoinColumnName1.setFocus();
				return false;
			}

			if (chkBidirectional.getSelection()) {
				status = EclipseIDEService.validateFieldName(txtReversePropertyName.getText());

				if (status.getSeverity() > IStatus.INFO) {
					MessageDialog.openInformation(getShell(), DLG_TITLE_NEW, status.getMessage());
					txtReversePropertyName.setFocus();
					return false;
				}

				reverseAssociationName = txtReversePropertyName.getText();
			}
			else {
				reverseAssociationName = null;

				try {
					new DBSynchService(db).validateIdentifier(joinColumnName2);
				}
				catch (final DBObjectValidationException e) {
					MessageDialog.openInformation(getShell(), DLG_TITLE_NEW, e.getMessage());
					txtJoinColumnName2.setFocus();
					return false;
				}

				try {
					new DBSynchService(db).validateTableName(sourceTable.getSchemaName(), sourceTable.getCatalogName(), joinTableName);
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
			}

			association.setTag(AssociationTagEnumeration.valueOf(cboTag.getItem(cboTag.getSelectionIndex())));
		}

		association.setCascadeMerge(chkCascadeMerge.getSelection());
		association.setCascadePersist(chkCascadePersist.getSelection());
		association.setCascadeRefresh(chkCascadeRefresh.getSelection());
		association.setCascadeRemove(chkCascadeRemove.getSelection());
		association.setFetchTypeEager(chkFetchTypeEager.getSelection());
		association.setName(txtAssociationName.getText());
		association.setInternalComment(txtInternalComment.getText());
		association.setUserComment(txtUserComment.getText());
		association.setOwner(true);

		return true;
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
	 * @return the association
	 */
	public OneToManyAssociation getAssociation() {
		return association;
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

	/**
	 * @return the default name of the owning side of the association
	 */
	private String createDefaultAssociationName() {
		return target.getNamePlural().substring(0, 1).toLowerCase() + target.getNamePlural().substring(1);
	}

	/**
	 * @return the default name of the reverse side of the association
	 */
	private String createReversePropertyName() {
		return source.getName().substring(0, 1).toLowerCase() + source.getName().substring(1);
	}

}
