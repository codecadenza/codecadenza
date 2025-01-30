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

import net.codecadenza.eclipse.model.db.DBColumn;
import net.codecadenza.eclipse.model.db.DBTable;
import net.codecadenza.eclipse.model.db.Database;
import net.codecadenza.eclipse.model.db.DbFactory;
import net.codecadenza.eclipse.model.domain.AssociationTagEnumeration;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.OneToOneAssociation;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaDialog;
import net.codecadenza.eclipse.tools.dbsync.util.DBObjectValidationException;
import net.codecadenza.eclipse.tools.dbsync.util.DBSynchService;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
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
 * Dialog for creating and maintaining one-to-one associations
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditOneToOneAssociationDialog extends CodeCadenzaDialog {
	private static final String DLG_TITLE_EDIT = "Edit one-to-one association";
	private static final String DLG_TITLE_NEW = "Create new one-to-one association";

	private Text txtDBColumnName;
	private Text txtAssociationName;
	private Button chkCascadePersist;
	private Button chkCascadeMerge;
	private Button chkCascadeRefresh;
	private Button chkCascadeRemove;
	private Button chkFetchTypeEager;
	private Button chkAddUniqueKey;
	private final DomainObject target;
	private final OneToOneAssociation association;
	private Text txtReverseAssocName;
	private final boolean doEdit;
	private Button chkBidirectional;
	private final DomainObject source;
	private Combo cboTag;
	private Label lblReverseAssocName;
	private boolean addUniqueKey;
	private String reverseAssociationName;
	private Text txtInternalComment;
	private Text txtUserComment;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param doEdit
	 * @param source
	 * @param target
	 * @param association
	 */
	public EditOneToOneAssociationDialog(Shell parentShell, boolean doEdit, DomainObject source, DomainObject target,
			OneToOneAssociation association) {
		super(parentShell);

		this.doEdit = doEdit;
		this.source = source;
		this.target = target;
		this.association = association;
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

		txtAssociationName.addKeyListener(new KeyListener() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.KeyListener#keyReleased(org.eclipse.swt.events.KeyEvent)
			 */
			@Override
			public void keyReleased(KeyEvent e) {
				txtDBColumnName.setText(EclipseIDEService.buildDefaultColumnName(txtAssociationName.getText()));
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.KeyListener#keyPressed(org.eclipse.swt.events.KeyEvent)
			 */
			@Override
			public void keyPressed(KeyEvent e) {
				// No implementation required!
			}
		});

		final var lblDBColumnName = new Label(panDialogArea, SWT.NONE);
		lblDBColumnName.setText("Join column name:");

		txtDBColumnName = new Text(panDialogArea, SWT.BORDER);
		txtDBColumnName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblFetchTypeEager = new Label(panDialogArea, SWT.NONE);
		lblFetchTypeEager.setText("Fetch type eager:");

		chkFetchTypeEager = new Button(panDialogArea, SWT.CHECK);

		new Label(panDialogArea, SWT.None);
		new Label(panDialogArea, SWT.None);

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
			final var lblTag = new Label(panDialogArea, SWT.NONE);
			lblTag.setText("Tag:");

			cboTag = new Combo(panDialogArea, SWT.READ_ONLY);
			cboTag.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

			source.getValidAssociationTags(null).forEach(cboTag::add);

			cboTag.select(0);

			final var lblAddUniqueKey = new Label(panDialogArea, SWT.NONE);
			lblAddUniqueKey.setText("Add unique key:");

			chkAddUniqueKey = new Button(panDialogArea, SWT.CHECK);
			chkAddUniqueKey.setSelection(true);
		}

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
				txtReverseAssocName.setEnabled(chkBidirectional.getSelection());
				lblReverseAssocName.setEnabled(chkBidirectional.getSelection());
			}
		});

		lblReverseAssocName = new Label(panDialogArea, SWT.NONE);
		lblReverseAssocName.setText("Reverse name:");

		txtReverseAssocName = new Text(panDialogArea, SWT.BORDER);
		txtReverseAssocName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

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
			final String assocName = target.getName().substring(0, 1).toLowerCase() + target.getName().substring(1);

			txtAssociationName.setText(assocName);
			txtDBColumnName.setText(EclipseIDEService.buildDefaultColumnName(txtAssociationName.getText()));
			chkCascadeMerge.setSelection(true);
			chkCascadePersist.setSelection(true);
			chkCascadeRefresh.setSelection(true);
			chkCascadeRemove.setSelection(true);
			chkBidirectional.setSelection(true);
			txtReverseAssocName.setText(createReverseName());
		}
		else {
			chkCascadeMerge.setSelection(association.isCascadeMerge());
			chkCascadePersist.setSelection(association.isCascadePersist());
			chkCascadeRefresh.setSelection(association.isCascadeRefresh());
			chkCascadeRemove.setSelection(association.isCascadeRemove());
			txtAssociationName.setText(association.getName());
			chkFetchTypeEager.setSelection(association.isFetchTypeEager());
			chkBidirectional.setSelection(association.isBidirectional());
			txtInternalComment.setText(association.getInternalComment() == null ? "" : association.getInternalComment());
			txtUserComment.setText(association.getUserComment() == null ? "" : association.getUserComment());
			txtDBColumnName.setEnabled(false);
			txtAssociationName.setEnabled(false);
			txtReverseAssocName.setEnabled(false);
			chkBidirectional.setEnabled(false);

			if (association.isBidirectional())
				txtReverseAssocName.setText(association.getReverseAssociation().getName());

			if (association.getColumn() != null)
				txtDBColumnName.setText(association.getColumn().getName());

			if (!association.isOwner()) {
				chkCascadePersist.setEnabled(false);
				chkCascadeMerge.setEnabled(false);
				chkCascadeRefresh.setEnabled(false);
				chkCascadeRemove.setEnabled(false);
				chkFetchTypeEager.setEnabled(false);
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
		if (doEdit && !association.isOwner()) {
			association.setInternalComment(txtInternalComment.getText());
			association.setUserComment(txtUserComment.getText());
			return true;
		}

		if (!doEdit) {
			IStatus status = EclipseIDEService.validateFieldName(txtAssociationName.getText());

			if (status.getSeverity() > IStatus.INFO) {
				MessageDialog.openInformation(getShell(), DLG_TITLE_NEW, status.getMessage());
				txtAssociationName.setFocus();
				return false;
			}

			try {
				final Database db = target.getNamespace().getProject().getDatabase();
				DBTable table = association.getDomainObject().getDatabaseTable();

				if (table == null)
					table = association.getDomainObject().getRootParentDomainObject(false).getDatabaseTable();

				new DBSynchService(db).validateColumnName(table, txtDBColumnName.getText());
			}
			catch (final DBObjectValidationException ex) {
				MessageDialog.openInformation(getShell(), DLG_TITLE_NEW, ex.getMessage());

				txtDBColumnName.setFocus();
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

			association.setName(txtAssociationName.getText());
			association.setOptional(false);

			// If the one-to-one association references the same object it makes sense that the association is optional!
			if (association.getTarget().equals(association.getDomainObject()))
				association.setOptional(true);

			addUniqueKey = chkAddUniqueKey.getSelection();

			final DBColumn col = DbFactory.eINSTANCE.createDBColumn();

			association.setColumn(col);
			association.setOwner(true);
			association.setTag(AssociationTagEnumeration.valueOf(cboTag.getItem(cboTag.getSelectionIndex())));
			association.getColumn().setName(txtDBColumnName.getText());
		}

		association.setCascadeMerge(chkCascadeMerge.getSelection());
		association.setCascadePersist(chkCascadePersist.getSelection());
		association.setCascadeRefresh(chkCascadeRefresh.getSelection());
		association.setCascadeRemove(chkCascadeRemove.getSelection());
		association.setFetchTypeEager(chkFetchTypeEager.getSelection());
		association.setInternalComment(txtInternalComment.getText());
		association.setUserComment(txtUserComment.getText());

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
	 * @return the association
	 */
	public OneToOneAssociation getAssociation() {
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
	 * @return the default name of the reverse property
	 */
	private String createReverseName() {
		return source.getName().substring(0, 1).toLowerCase() + source.getName().substring(1);
	}

}
