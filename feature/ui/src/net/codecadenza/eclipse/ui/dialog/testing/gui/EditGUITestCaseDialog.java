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
package net.codecadenza.eclipse.ui.dialog.testing.gui;

import static net.codecadenza.eclipse.shared.Constants.IMG_CLIENT_TEXT;
import static net.codecadenza.eclipse.shared.Constants.IMG_COLUMN;
import static net.codecadenza.eclipse.shared.Constants.IMG_INDEX;
import static net.codecadenza.eclipse.shared.Constants.IMG_KEY;
import static net.codecadenza.eclipse.shared.Constants.IMG_PANEL;
import static net.codecadenza.eclipse.shared.Constants.IMG_UPLOAD;
import static net.codecadenza.eclipse.shared.Constants.IMG_WINDOW_READ;
import static net.codecadenza.eclipse.shared.Constants.MIN_FILTER_LENGTH;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.regex.Pattern;
import net.codecadenza.eclipse.model.client.ActionType;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.testing.AbstractTestModule;
import net.codecadenza.eclipse.model.testing.GUITestAction;
import net.codecadenza.eclipse.model.testing.GUITestActionType;
import net.codecadenza.eclipse.model.testing.GUITestCase;
import net.codecadenza.eclipse.model.testing.GUITestData;
import net.codecadenza.eclipse.model.testing.GUITestDataType;
import net.codecadenza.eclipse.model.testing.TestingFactory;
import net.codecadenza.eclipse.resource.CodeCadenzaResourcePlugin;
import net.codecadenza.eclipse.service.testing.gui.GUITestCaseService;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaTitleAreaDialog;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.tools.tdp.GUITestDataProposalService;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.eclipse.ui.dialog.testing.gui.editor.GUITestFormEditorDialog;
import net.codecadenza.eclipse.ui.dialog.testing.gui.editor.action.ActionResultDialog;
import net.codecadenza.eclipse.ui.dialog.testing.gui.editor.action.LoginDialog;
import net.codecadenza.eclipse.ui.dialog.testing.gui.editor.action.RowCountDialog;
import net.codecadenza.eclipse.ui.dialog.testing.gui.editor.action.RowSelectionDialog;
import net.codecadenza.eclipse.ui.dialog.testing.gui.editor.action.SearchInputDialog;
import net.codecadenza.eclipse.ui.dialog.testing.gui.util.GUITestActionUtil;
import net.codecadenza.runtime.richclient.eclipse.widget.AbstractProposalTextField;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.jdt.ui.JavaUI;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;

/**
 * <p>
 * Dialog for maintaining GUI test case objects
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditGUITestCaseDialog extends CodeCadenzaTitleAreaDialog {
	private static final String NEW_LINE_SYMBOL = "↳";
	private static final String DLG_TITLE_CREATE = "Create new GUI test case";
	private static final String DLG_TITLE_EDIT = "Edit GUI test case";
	private static final String DEFAULT_TEST_CASE_NAME = "New";
	private static final Pattern NEW_LINE_PATTERN = Pattern.compile("\r\n|\r|\n");
	private static final char MOVE_UP = '+';
	private static final char MOVE_DOWN = '-';

	private final Project project;
	private final AbstractTestModule testModule;
	private final GUITestCase testCase;
	private Text txtName;
	private Text txtComment;
	private Button chkOpenByNavigator;
	private Label lblObjectId;
	private AbstractProposalTextField<String> txtObjectId;
	private AbstractProposalTextField<Form> txtForm;
	private Button cmdAddAction;
	private Tree treeActions;
	private Menu menuTreeActions;
	private GUITestAction selectedTestAction;
	private boolean editMode;
	private String title = DLG_TITLE_CREATE;
	private boolean expandTreeItems;
	private boolean enableDatabaseLookup;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param testCase
	 */
	public EditGUITestCaseDialog(Shell parentShell, GUITestCase testCase) {
		super(parentShell);

		this.testCase = testCase;
		this.testModule = testCase.getTestModule();
		this.project = testCase.getNamespace().getProject();
		this.title = DLG_TITLE_EDIT;
		this.editMode = true;
	}

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param testModule
	 */
	public EditGUITestCaseDialog(Shell parentShell, AbstractTestModule testModule) {
		super(parentShell);

		this.testModule = testModule;
		this.testCase = TestingFactory.eINSTANCE.createGUITestCase();
		this.project = testModule.getProject();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.TitleAreaDialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent);

		final var groupBasicData = new Group(panDialogArea, SWT.NONE);
		groupBasicData.setText("Basic data");
		groupBasicData.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
		groupBasicData.setLayout(new GridLayout(2, false));

		final var lblName = new Label(groupBasicData, SWT.NONE);
		lblName.setText("Name:");

		txtName = new Text(groupBasicData, SWT.BORDER);
		txtName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblComment = new Label(groupBasicData, SWT.NONE);
		lblComment.setText("Comment:");

		final var gdComment = new GridData(SWT.FILL, SWT.CENTER, true, false);
		gdComment.heightHint = 50;

		txtComment = new Text(groupBasicData, SWT.V_SCROLL | SWT.MULTI | SWT.BORDER | SWT.H_SCROLL);
		txtComment.setLayoutData(gdComment);

		final var groupAddNew = new Group(panDialogArea, SWT.NONE);
		groupAddNew.setLayout(new GridLayout(4, false));
		groupAddNew.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		groupAddNew.setText("Add new action");

		final var lblNewFormAction = new Label(groupAddNew, SWT.NONE);
		lblNewFormAction.setText("Open form");

		txtForm = new AbstractProposalTextField<>(groupAddNew, SWT.BORDER, MIN_FILTER_LENGTH) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.AbstractProposalTextField#getProposalLabel(java.lang. Object)
			 */
			@Override
			public String getProposalLabel(Form form) {
				return form.getName();
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.AbstractProposalTextField#getProposalData(java.lang.String)
			 */
			@Override
			public Collection<Form> getProposalData(String filter) {
				// Forms of type 'LOV' and 'TREE_VIEW' are generally not supported!
				return project.getAllFormsOfProject().stream().filter(e -> e.getName().toLowerCase().contains(filter.toLowerCase())
						&& e.getFormType() != FormTypeEnumeration.LOV && e.getFormType() != FormTypeEnumeration.TREE_VIEW).toList();
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.AbstractProposalTextField#onProposalAccepted(java.lang. Object)
			 */
			@Override
			public void onProposalAccepted(Form form) {
				final FormTypeEnumeration formType = form.getFormType();

				chkOpenByNavigator.setEnabled(false);
				txtObjectId.getControl().setEnabled(true);
				lblObjectId.setEnabled(true);
				cmdAddAction.setEnabled(true);
				txtObjectId.getControl().setText("");

				if (formType == FormTypeEnumeration.SEARCHABLE_VIEW || form.getFormType() == FormTypeEnumeration.SIMPLE_VIEW) {
					txtObjectId.getControl().setEnabled(false);
					lblObjectId.setEnabled(false);
					chkOpenByNavigator.setEnabled(true);
				}
				else if (form.getFormType() == FormTypeEnumeration.CREATE) {
					txtObjectId.getControl().setEnabled(false);
					lblObjectId.setEnabled(false);
				}
			}
		};

		txtForm.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		chkOpenByNavigator = new Button(groupAddNew, SWT.CHECK);
		chkOpenByNavigator.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false));
		chkOpenByNavigator.setText("by using main navigator");
		chkOpenByNavigator.setEnabled(false);

		final var panAddButton = new Composite(groupAddNew, SWT.NONE);
		panAddButton.setLayout(new GridLayout());
		panAddButton.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false, 1, 2));

		final var gdAddAction = new GridData(SWT.CENTER, SWT.CENTER, false, false);
		gdAddAction.heightHint = 40;
		gdAddAction.widthHint = 60;

		cmdAddAction = new Button(panAddButton, SWT.PUSH);
		cmdAddAction.setText("Add");
		cmdAddAction.setLayoutData(gdAddAction);
		cmdAddAction.setEnabled(false);

		cmdAddAction.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				createPageOpenAction();
			}
		});

		lblObjectId = new Label(groupAddNew, SWT.NONE);
		lblObjectId.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false));
		lblObjectId.setText("with object ID");
		lblObjectId.setEnabled(false);

		txtObjectId = new AbstractProposalTextField<>(groupAddNew, SWT.BORDER, MIN_FILTER_LENGTH) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.AbstractProposalTextField#getProposalData(java.lang.String)
			 */
			@Override
			public Collection<String> getProposalData(String filter) {
				if (filter.isEmpty())
					return Collections.emptyList();

				final Form selectedForm = txtForm.getSelectedItem();

				if (selectedForm == null)
					return Collections.emptyList();

				// Determine an appropriate form field in order to provide test data proposals
				if (selectedForm.getFormType() == FormTypeEnumeration.ADD) {
					for (final FormField formField : txtForm.getSelectedItem().getAllFormFields())
						if (formField.getFieldType() == FormFieldTypeEnumeration.SELECTION_BY_PARENT_FORM)
							return GUITestDataProposalService.searchProposals(filter, formField, enableDatabaseLookup);
				}
				else
					for (final FormField formField : txtForm.getSelectedItem().getAllFormFields()) {
						if (formField.getDTOAttribute() == null || formField.getDTOAttribute().getAssociation() != null)
							continue;

						if (formField.getDTOAttribute().getDomainAttribute() != null
								&& formField.getDTOAttribute().getDomainAttribute().isPk())
							return GUITestDataProposalService.searchProposals(filter, formField, enableDatabaseLookup);
					}

				return Collections.emptyList();
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.AbstractProposalTextField#getProposalLabel(java.lang. Object)
			 */
			@Override
			public String getProposalLabel(String element) {
				return element;
			}
		};

		txtObjectId.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtObjectId.getControl().setEnabled(false);

		final var gdActionList = new GridData(SWT.FILL, SWT.FILL, true, true);
		gdActionList.heightHint = 400;

		final var groupActionList = new Group(panDialogArea, SWT.NONE);
		groupActionList.setLayout(new GridLayout());
		groupActionList.setLayoutData(gdActionList);
		groupActionList.setText("Existing test actions");

		treeActions = new Tree(groupActionList, SWT.NONE);
		treeActions.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		treeActions.addMouseListener(new MouseAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.MouseAdapter#mouseDoubleClick(org.eclipse.swt.events.MouseEvent)
			 */
			@Override
			public void mouseDoubleClick(MouseEvent e) {
				editTestAction(getSelectedTestAction());
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.MouseAdapter#mouseDown(org.eclipse.swt.events.MouseEvent)
			 */
			@Override
			public void mouseDown(MouseEvent e) {
				selectedTestAction = getSelectedTestAction();
			}
		});

		treeActions.addMenuDetectListener(_ -> {
			treeActions.setMenu(null);

			// Show the menu for test actions only!
			if (getSelectedTestAction() == null)
				return;

			treeActions.setMenu(menuTreeActions);
		});

		treeActions.addKeyListener(new KeyAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.KeyAdapter#keyPressed(org.eclipse.swt.events.KeyEvent)
			 */
			@Override
			public void keyPressed(KeyEvent e) {
				selectedTestAction = getSelectedTestAction();

				if (selectedTestAction == null)
					return;

				if (e.character == MOVE_UP)
					moveTestAction(getSelectedTestAction(), true);
				else if (e.character == MOVE_DOWN)
					moveTestAction(getSelectedTestAction(), false);
			}
		});

		menuTreeActions = new Menu(treeActions);

		final var mniEdit = new MenuItem(menuTreeActions, SWT.NONE);
		mniEdit.setText("Edit test action");

		mniEdit.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				editTestAction(getSelectedTestAction());
			}
		});

		final var mniEditComment = new MenuItem(menuTreeActions, SWT.NONE);
		mniEditComment.setText("Edit comment");

		mniEditComment.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				editTestActionComment(getSelectedTestAction());
			}
		});

		final var mniPageTitle = new MenuItem(menuTreeActions, SWT.NONE);
		mniPageTitle.setText("Edit expected page title");

		mniPageTitle.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				editPageTitleValidation(getSelectedTestAction());
			}
		});

		final var mniActionResult = new MenuItem(menuTreeActions, SWT.NONE);
		mniActionResult.setText("Edit test action result");

		mniActionResult.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				editActionResult(getSelectedTestAction());
			}
		});

		final var mniOpenEditor = new MenuItem(menuTreeActions, SWT.NONE);
		mniOpenEditor.setText("Open GUI test form editor");

		mniOpenEditor.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				openGUITestFormEditor(getSelectedTestAction());

				refreshTestActionTree();
			}
		});

		final var mniDelay = new MenuItem(menuTreeActions, SWT.CASCADE);
		mniDelay.setText("Delay");

		final var mnuSubDelay = new Menu(getShell(), SWT.DROP_DOWN);
		mniDelay.setMenu(mnuSubDelay);

		final var mniDelayBefore = new MenuItem(mnuSubDelay, SWT.NONE);
		mniDelayBefore.setText("Before");

		mniDelayBefore.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				editDelay(getSelectedTestAction(), true);
			}
		});

		final var mniDelayAfter = new MenuItem(mnuSubDelay, SWT.NONE);
		mniDelayAfter.setText("After");

		mniDelayAfter.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				editDelay(getSelectedTestAction(), false);
			}
		});

		final var mniDelete = new MenuItem(menuTreeActions, SWT.NONE);
		mniDelete.setText("Delete test action");
		mniDelete.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_TOOL_DELETE));

		mniDelete.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				deleteTestAction(getSelectedTestAction());
			}
		});

		new MenuItem(menuTreeActions, SWT.SEPARATOR);

		final var mniMoveUp = new MenuItem(menuTreeActions, SWT.NONE);
		mniMoveUp.setText("Move up");
		mniMoveUp.setAccelerator(MOVE_UP);

		mniMoveUp.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				moveTestAction(getSelectedTestAction(), true);
			}
		});

		final var mniMoveDown = new MenuItem(menuTreeActions, SWT.NONE);
		mniMoveDown.setText("Move down");
		mniMoveDown.setAccelerator(MOVE_DOWN);

		mniMoveDown.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				moveTestAction(getSelectedTestAction(), false);
			}
		});

		new MenuItem(menuTreeActions, SWT.SEPARATOR);

		final var mniLogin = new MenuItem(menuTreeActions, SWT.NONE);
		mniLogin.setText("Add login action");

		mniLogin.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				addLoginAction(getSelectedTestAction());
			}
		});

		final var mniLogout = new MenuItem(menuTreeActions, SWT.NONE);
		mniLogout.setText("Add logout action");

		mniLogout.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				addLogoutAction(getSelectedTestAction());
			}
		});

		new MenuItem(menuTreeActions, SWT.SEPARATOR);

		final var mniExpandTree = new MenuItem(menuTreeActions, SWT.NONE);
		mniExpandTree.setText("Expand tree");

		mniExpandTree.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				expandTreeItems = true;

				refreshTestActionTree();
			}
		});

		final var mniCollapseTree = new MenuItem(menuTreeActions, SWT.NONE);
		mniCollapseTree.setText("Collapse tree");

		mniCollapseTree.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				expandTreeItems = false;

				refreshTestActionTree();
			}
		});

		// Fill the dialog
		if (editMode) {
			txtName.setText(testCase.getName());
			txtComment.setText(testCase.getComment());

			refreshTestActionTree();
		}
		else
			txtName.setText(DEFAULT_TEST_CASE_NAME + testModule.getTestCaseSuffix());

		setTitle(title);

		final long start = System.currentTimeMillis();

		// Test if a connection to the target database can be established
		enableDatabaseLookup = GUITestDataProposalService.enableDatabaseLookup(project);

		final long end = System.currentTimeMillis();

		if (enableDatabaseLookup)
			setMessage("The database connection for loading test data proposals has been established in "
					+ new DecimalFormat("0.00").format((end - start) / 1000.0) + " seconds");
		else
			setMessage(
					"The database connection could not be established. Test data proposals can only be loaded from existing test cases!");

		return panDialogArea;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
	 */
	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);

		newShell.setText(title);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID) {
			if (!checkInput())
				return;

			try {
				saveTestCase();
			}
			catch (final Exception e) {
				CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(e);
				return;
			}
		}

		super.buttonPressed(buttonId);
	}

	/**
	 * Edit a given GUI test action by opening the respective dialog
	 * @param testAction
	 */
	private void editTestAction(GUITestAction testAction) {
		if (testAction == null)
			return;

		final GUITestActionType type = testAction.getType();

		if (type == GUITestActionType.ENTER_SEARCH_DATA || type == GUITestActionType.COUNT_RECORDS) {
			// Open the search input dialog
			new SearchInputDialog(getShell(), testAction, false, enableDatabaseLookup).open();
		}
		else if (type == GUITestActionType.OPEN_LOGIN_PAGE) {
			// Open the login dialog
			new LoginDialog(getShell(), project, testAction).open();
		}
		else if (type == GUITestActionType.VALIDATE_ROW_COUNT_EQUAL || type == GUITestActionType.VALIDATE_ROW_COUNT_GREATER
				|| type == GUITestActionType.VALIDATE_ROW_COUNT_SMALLER) {
			// Open the dialog for maintaining actions that validate the row count of view forms and grid panels
			new RowCountDialog(getShell(), testAction).open();
		}
		else if (type == GUITestActionType.SEARCH_ROW_ALL_PAGES || type == GUITestActionType.SEARCH_ROW_CURRENT_PAGE) {
			// Open the dialog for maintaining row selection actions
			new RowSelectionDialog(getShell(), project, testAction, enableDatabaseLookup).open();
		}
		else
			openGUITestFormEditor(testAction);

		refreshTestActionTree();
	}

	/**
	 * Edit a GUI test action comment
	 * @param testAction
	 */
	private void editTestActionComment(GUITestAction testAction) {
		if (testAction == null)
			return;

		// Open the dialog for editing the comment
		final var dlg = new InputDialog(getShell(), "Edit test action comment", "Enter the comment:", testAction.getComment(),
				newText -> {
					if (newText == null || newText.isEmpty())
						return "Please enter a comment!";

					return null;
				});

		final int returnCode = dlg.open();

		if (returnCode != Window.OK)
			return;

		testAction.setComment(dlg.getValue());

		refreshTestActionTree();
	}

	/**
	 * Open the GUI test form editor in order to either maintain test data or to define new test actions
	 * @param testAction
	 */
	private void openGUITestFormEditor(GUITestAction testAction) {
		if (testAction == null)
			return;

		final GUITestActionType type = testAction.getType();
		boolean maintainTestData = false;

		// The preview dialog should enable test data entry for suitable test actions only!
		if (type == GUITestActionType.VALIDATE_FORM_DATA || type == GUITestActionType.ENTER_FORM_DATA)
			maintainTestData = true;

		final var dlg = new GUITestFormEditorDialog(getShell(), testAction, testAction.getForm(), maintainTestData,
				enableDatabaseLookup);
		final int returnCode = dlg.open();

		if (returnCode != Window.OK)
			return;

		final int actionIndex = testCase.getTestActions().indexOf(testAction);
		final List<GUITestAction> testActions = dlg.getPreviewBuilder().getTestActions();

		if (type == GUITestActionType.VALIDATE_FORM_DATA || type == GUITestActionType.ENTER_FORM_DATA) {
			// New test actions must be performed before either entering or validating form data!
			testCase.getTestActions().addAll(actionIndex, testActions);
		}
		else if (type == GUITestActionType.PRESS_CANCEL_BUTTON || type == GUITestActionType.PRESS_OK_BUTTON) {
			int targetActionIndex = actionIndex;

			if (actionIndex > 0) {
				final GUITestAction predecessorAction = testCase.getTestActions().get(actionIndex - 1);
				final GUITestActionType predecessorType = predecessorAction.getType();

				if (predecessorType == GUITestActionType.VALIDATE_FORM_DATA || predecessorType == GUITestActionType.ENTER_FORM_DATA)
					targetActionIndex = actionIndex - 1;
			}

			testCase.getTestActions().addAll(targetActionIndex, testActions);
		}
		else if ((actionIndex + 1) < testCase.getTestActions().size()) {
			// In any other case, all new test actions should be executed after the currently selected action!
			testCase.getTestActions().addAll(actionIndex + 1, testActions);
		}
		else
			testCase.getTestActions().addAll(testActions);
	}

	/**
	 * Edit the delay for a GUI test action
	 * @param testAction
	 * @param before
	 */
	private void editDelay(GUITestAction testAction, boolean before) {
		if (testAction == null)
			return;

		String dialogTitle;
		String defaultValue = null;

		if (before) {
			dialogTitle = "Enter a delay before executing the test action";

			if (testAction.getDelayBefore() != null)
				defaultValue = Integer.toString(testAction.getDelayBefore());
		}
		else {
			dialogTitle = "Enter a delay after executing the test action";

			if (testAction.getDelayAfter() != null)
				defaultValue = Integer.toString(testAction.getDelayAfter());
		}

		// Open the dialog for editing the delay
		final var dlg = new InputDialog(getShell(), dialogTitle, "Enter the delay time in milliseconds:", defaultValue, newText -> {
			if (newText == null || newText.isEmpty())
				return null;

			try {
				Integer.parseInt(newText);
			}
			catch (final NumberFormatException e) {
				return e.getMessage();
			}

			return null;
		});

		final int returnCode = dlg.open();

		if (returnCode != Window.OK)
			return;

		final Integer delay = dlg.getValue() != null && !dlg.getValue().isEmpty() ? Integer.parseInt(dlg.getValue()) : null;

		if (before)
			testAction.setDelayBefore(delay);
		else
			testAction.setDelayAfter(delay);
	}

	/**
	 * Delete a GUI test action
	 * @param testAction
	 */
	private void deleteTestAction(GUITestAction testAction) {
		final var msgTitle = "Delete test action";

		if (testAction == null)
			return;

		final boolean doIt = MessageDialog.openConfirm(getShell(), msgTitle,
				"Do you really want to delete the selected test action?");

		if (!doIt)
			return;

		final int actionIndex = testCase.getTestActions().indexOf(testAction);
		final int selectionIndex = actionIndex > 0 ? actionIndex - 1 : 0;

		testCase.getTestActions().remove(testAction);

		// Select the predecessor test action in the tree view. If the test action has no predecessor the first test action will be
		// selected!
		if (!testCase.getTestActions().isEmpty())
			selectedTestAction = testCase.getTestActions().get(selectionIndex);

		refreshTestActionTree();
	}

	/**
	 * Create a new test action based on the values entered in the respective form fields
	 */
	private void createPageOpenAction() {
		if (txtForm.getSelectedItem() == null)
			return;

		setErrorMessage(null);

		final Form form = txtForm.getSelectedItem();
		final FormTypeEnumeration formType = form.getFormType();

		if (txtObjectId.getControl().getText().isEmpty() && (formType == FormTypeEnumeration.UPDATE
				|| formType == FormTypeEnumeration.READONLY || formType == FormTypeEnumeration.ADD)) {
			setErrorMessage("The selected form requires an object ID!");
			txtObjectId.getControl().setFocus();
			return;
		}

		final GUITestAction pageOpenAction = TestingFactory.eINSTANCE.createGUITestAction();
		pageOpenAction.setForm(form);

		if (!txtObjectId.getControl().getText().isEmpty()) {
			final GUITestData testData = TestingFactory.eINSTANCE.createGUITestData();
			testData.setType(GUITestDataType.OBJECT_ID);
			testData.setFilterValue(txtObjectId.getControl().getText());
			testData.setTestAction(pageOpenAction);
		}

		if (chkOpenByNavigator.getSelection())
			pageOpenAction.setType(GUITestActionType.OPEN_PAGE_BY_NAVIGATOR);
		else
			pageOpenAction.setType(GUITestActionType.OPEN_PAGE_DIRECT);

		// Create a comment
		GUITestActionUtil.initTestActionComment(pageOpenAction);

		if (formType == FormTypeEnumeration.SIMPLE_VIEW || formType == FormTypeEnumeration.SEARCHABLE_VIEW) {
			final var dlg = new GUITestFormEditorDialog(getShell(), pageOpenAction, form, false, enableDatabaseLookup);
			final int returnCode = dlg.open();

			if (returnCode != Window.OK)
				return;

			testCase.getTestActions().add(pageOpenAction);
			testCase.getTestActions().addAll(dlg.getPreviewBuilder().getTestActions());
		}
		else {
			testCase.getTestActions().add(pageOpenAction);
			testCase.getTestActions().addAll(GUITestActionUtil.initGUITestActionsOfForm(form, null, enableDatabaseLookup));
		}

		refreshTestActionTree();
	}

	/**
	 * Add a login test action
	 * @param selectedTestAction
	 */
	private void addLoginAction(GUITestAction selectedTestAction) {
		final var dlg = new LoginDialog(getShell(), project);
		final int returnCode = dlg.open();

		if (returnCode != Window.OK)
			return;

		if (selectedTestAction != null) {
			// If a test action has been selected the login action should be processed first!
			final int actionIndex = testCase.getTestActions().indexOf(selectedTestAction);

			testCase.getTestActions().add(actionIndex, dlg.getLoginAction());
		}
		else
			testCase.getTestActions().add(dlg.getLoginAction());

		refreshTestActionTree();
	}

	/**
	 * Add a logout test action
	 * @param selectedTestAction
	 */
	private void addLogoutAction(GUITestAction selectedTestAction) {
		if (selectedTestAction == null || selectedTestAction.getForm() == null) {
			MessageDialog.openInformation(getShell(), "Add logout action", "A predecessor test action must be selected!");
			return;
		}

		final GUITestAction logoutAction = TestingFactory.eINSTANCE.createGUITestAction();
		logoutAction.setForm(selectedTestAction.getForm());
		logoutAction.setType(GUITestActionType.PERFORM_LOGOUT);

		// Create a comment
		GUITestActionUtil.initTestActionComment(logoutAction);

		final int actionIndex = testCase.getTestActions().indexOf(selectedTestAction);

		// Add the new action after the currently selected action
		if ((actionIndex + 1) < testCase.getTestActions().size())
			testCase.getTestActions().add(actionIndex + 1, logoutAction);
		else
			testCase.getTestActions().add(logoutAction);

		refreshTestActionTree();
	}

	/**
	 * Maintain the result of the selected action
	 * @param selectedTestAction
	 */
	private void editActionResult(GUITestAction selectedTestAction) {
		if (selectedTestAction == null) {
			MessageDialog.openInformation(getShell(), "Action result", "A test action must be selected!");
			return;
		}

		final GUITestActionType type = selectedTestAction.getType();

		// For some test action types the definition of an action result makes no sense!
		final boolean invalidType = type == GUITestActionType.VALIDATE_FORM_DATA || type == GUITestActionType.ENTER_FORM_DATA
				|| type == GUITestActionType.SEARCH_ROW_ALL_PAGES || type == GUITestActionType.SEARCH_ROW_CURRENT_PAGE
				|| type == GUITestActionType.VALIDATE_ROW_COUNT_EQUAL || type == GUITestActionType.VALIDATE_ROW_COUNT_GREATER
				|| type == GUITestActionType.VALIDATE_ROW_COUNT_SMALLER;

		if (invalidType) {
			MessageDialog.openInformation(getShell(), "Action result",
					"Creating an action result is not supported for the selected test action!");
			return;
		}

		new ActionResultDialog(getShell(), project, selectedTestAction).open();
	}

	/**
	 * Create, update or delete the test data object that contains information regarding page title validation
	 * @param selectedTestAction
	 */
	private void editPageTitleValidation(GUITestAction selectedTestAction) {
		final var dialogTitle = "Expected page title";

		if (selectedTestAction == null || selectedTestAction.getForm() == null) {
			MessageDialog.openInformation(getShell(), dialogTitle, "A test action must be selected!");
			return;
		}

		if (selectedTestAction.getType() != GUITestActionType.OPEN_PAGE_BY_NAVIGATOR
				&& selectedTestAction.getType() != GUITestActionType.OPEN_PAGE_DIRECT) {
			MessageDialog.openInformation(getShell(), dialogTitle,
					"The page title validation cannot be added to the selected test action!");
			return;
		}

		// Search for existing test data
		final GUITestData objectIdTestData = selectedTestAction.getTestDataByType(GUITestDataType.OBJECT_ID);
		GUITestData titleTestData = selectedTestAction.getTestDataByType(GUITestDataType.PAGE_TITLE);

		var pageTitleProposal = "";

		if (titleTestData == null) {
			if (objectIdTestData != null)
				pageTitleProposal = generatePageTitle(selectedTestAction.getForm(), objectIdTestData.getFilterValue());
			else
				pageTitleProposal = generatePageTitle(selectedTestAction.getForm(), null);
		}
		else
			pageTitleProposal = titleTestData.getExpectedValue();

		final var dlg = new InputDialog(getShell(), dialogTitle, "Enter the expected page title:", pageTitleProposal, null);
		final int returnCode = dlg.open();

		if (returnCode != Window.OK)
			return;

		// Delete the existing test data object if the provided string is either null or empty!
		if (dlg.getValue() == null || dlg.getValue().isEmpty() && titleTestData != null) {
			selectedTestAction.getTestData().remove(titleTestData);
			return;
		}

		if (titleTestData == null) {
			// Create a new test data object for validating the page title
			titleTestData = TestingFactory.eINSTANCE.createGUITestData();
			titleTestData.setType(GUITestDataType.PAGE_TITLE);
			titleTestData.setTestAction(selectedTestAction);
		}

		titleTestData.setExpectedValue(dlg.getValue());
	}

	/**
	 * Change the position of a given test action in the internal list for changing the invocation order within a test case
	 * @param selectedTestAction
	 * @param moveUp
	 */
	private void moveTestAction(GUITestAction selectedTestAction, boolean moveUp) {
		if (selectedTestAction == null)
			return;

		int currentIndex = testCase.getTestActions().indexOf(selectedTestAction);

		if (moveUp) {
			if (currentIndex > 0)
				testCase.getTestActions().move(--currentIndex, selectedTestAction);
		}
		else if ((currentIndex + 1) < testCase.getTestActions().size())
			testCase.getTestActions().move(++currentIndex, selectedTestAction);

		refreshTestActionTree();
	}

	/**
	 * Create a proposal for the page title
	 * @param selectedForm
	 * @param objectId
	 * @return the page title
	 */
	private String generatePageTitle(Form selectedForm, String objectId) {
		if (selectedForm == null || selectedForm.getTitle() == null)
			return "";

		if (selectedForm.getFormType() == FormTypeEnumeration.READONLY || selectedForm.getFormType() == FormTypeEnumeration.UPDATE) {
			if (objectId == null || objectId.isEmpty())
				return selectedForm.getTitle();

			return selectedForm.getTitle() + " '" + objectId + "'";
		}

		return selectedForm.getTitle();
	}

	/**
	 * Validate the user input
	 * @return true if the validation was successful
	 */
	private boolean checkInput() {
		final IStatus status = EclipseIDEService.validateJavaTypeName(txtName.getText());

		if (status.getSeverity() > IStatus.INFO) {
			setErrorMessage(status.getMessage());
			txtName.setFocus();
			return false;
		}

		// Check if another object with the same name already exists
		for (final JavaType type : testModule.getNamespace().getJavaTypes()) {
			if (editMode && type.equals(testCase) && testCase.getName().equals(txtName.getText()))
				continue;

			if (type.getName().equals(txtName.getText())) {
				setErrorMessage("A class with the same name already exists in this package!");
				txtName.setFocus();
				return false;
			}
		}

		return true;
	}

	/**
	 * Save the test case
	 * @throws Exception if the save operation has failed
	 */
	private void saveTestCase() throws Exception {
		final var guiTestCaseService = new GUITestCaseService(project);

		if (editMode) {
			// Check if the test case must be renamed
			if (!testCase.getName().equals(txtName.getText()))
				guiTestCaseService.renameTestCase(testCase, txtName.getText());
		}
		else
			testCase.setNamespace(testModule.getNamespace());

		testCase.setName(txtName.getText());
		testCase.setComment(txtComment.getText());

		// Remove unnecessary test data
		removeUnnecessaryTestData();

		final Resource eResource = testModule.getNamespace().eResource();

		if (!eResource.getContents().contains(testCase))
			eResource.getContents().add(testCase);

		EclipseIDEService.saveProjectMetaData(project);

		guiTestCaseService.rebuildTestCaseSourceFiles(testCase);
	}

	/**
	 * Remove test data objects that don't contain useful data
	 */
	private void removeUnnecessaryTestData() {
		testCase.getTestActions().forEach(testAction -> {
			final GUITestActionType type = testAction.getType();
			final var itemsToRemove = new ArrayList<GUITestData>();

			if (type == GUITestActionType.ENTER_SEARCH_DATA || type == GUITestActionType.COUNT_RECORDS) {
				for (final GUITestData testData : testAction.getTestData())
					if (testData.getNewValue() == null || testData.getNewValue().isEmpty())
						itemsToRemove.add(testData);
			}
			else if (type == GUITestActionType.VALIDATE_FORM_DATA || type == GUITestActionType.ENTER_FORM_DATA) {
				for (final GUITestData testData : testAction.getTestData())
					if (testData.getNewValue() == null && testData.getExpectedValue() == null && testData.getFilterValue() == null)
						itemsToRemove.add(testData);
			}

			testAction.getTestData().removeAll(itemsToRemove);
		});
	}

	/**
	 * @return the selected GUI test action or null if no respective item is selected
	 */
	private GUITestAction getSelectedTestAction() {
		TreeItem selItem = null;

		final TreeItem[] selItems = treeActions.getSelection();

		for (final TreeItem item : selItems)
			selItem = item;

		if (selItem == null)
			return null;

		if (selItem.getData() instanceof final GUITestAction guiTestAction)
			return guiTestAction;

		return null;
	}

	/**
	 * Refresh the GUI test action tree view
	 */
	private void refreshTestActionTree() {
		treeActions.removeAll();

		// Add all GUI test actions
		for (final GUITestAction testAction : testCase.getTestActions()) {
			final var itemAction = new TreeItem(treeActions, SWT.NONE);
			itemAction.setText(testAction.getComment());
			itemAction.setData(testAction);
			itemAction.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));

			// Test if this item should be selected!
			if (testAction.equals(selectedTestAction))
				treeActions.setSelection(itemAction);

			final var itemType = new TreeItem(itemAction, SWT.NONE);
			itemType.setText("Action type: " + testAction.getType().getName());
			itemType.setImage(JavaUI.getSharedImages().getImage(org.eclipse.jdt.ui.ISharedImages.IMG_OBJS_PUBLIC));

			if (testAction.getForm() != null) {
				final var itemForm = new TreeItem(itemAction, SWT.NONE);
				itemForm.setText("Form: " + testAction.getForm().getName());
				itemForm.setImage(CodeCadenzaResourcePlugin.getImage(IMG_WINDOW_READ));
			}

			if (testAction.getFormPanel() != null) {
				final var itemPanel = new TreeItem(itemAction, SWT.NONE);
				itemPanel.setText("Form panel: " + testAction.getFormPanel().getName());
				itemPanel.setImage(CodeCadenzaResourcePlugin.getImage(IMG_PANEL));
			}

			if (testAction.getFormAction() != null) {
				final var itemPanel = new TreeItem(itemAction, SWT.NONE);
				itemPanel.setText("Form action: " + testAction.getFormAction().getName());
				itemPanel.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_TOOL_COPY));
			}

			itemAction.setExpanded(expandTreeItems);

			// Add a test data container item only if the respective data is available!
			if (testAction.getTestData().isEmpty())
				continue;

			addTestDataToTree(testAction, itemAction);
		}
	}

	/**
	 * Add the test data to the tree view
	 * @param testAction
	 * @param itemAction
	 */
	private void addTestDataToTree(GUITestAction testAction, TreeItem itemAction) {
		final var itemTestDataFolder = new TreeItem(itemAction, SWT.NONE);
		itemTestDataFolder.setText("Test data");
		itemTestDataFolder.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));

		testAction.getTestData().forEach(testData -> {
			if (testData.getFormField() != null) {
				if (testData.getNewValue() != null) {
					final var itemTestData = new TreeItem(itemTestDataFolder, SWT.NONE);
					final String value = NEW_LINE_PATTERN.matcher(testData.getNewValue()).replaceAll(NEW_LINE_SYMBOL);

					itemTestData.setText("Enter '" + value + "' into field '" + testData.getFormField().getLabel() + "'");
					itemTestData.setImage(CodeCadenzaResourcePlugin.getImage(IMG_CLIENT_TEXT));
				}

				if (testData.getExpectedValue() != null) {
					final var itemTestData = new TreeItem(itemTestDataFolder, SWT.NONE);
					final String value = NEW_LINE_PATTERN.matcher(testData.getExpectedValue()).replaceAll(NEW_LINE_SYMBOL);

					itemTestData.setText("Validate value '" + value + "' of field '" + testData.getFormField().getLabel() + "'");
					itemTestData.setImage(CodeCadenzaResourcePlugin.getImage(IMG_CLIENT_TEXT));
				}

				if (testData.getFilterValue() != null) {
					final var itemTestData = new TreeItem(itemTestDataFolder, SWT.NONE);
					final String value = NEW_LINE_PATTERN.matcher(testData.getFilterValue()).replaceAll(NEW_LINE_SYMBOL);

					itemTestData.setText("Enter '" + value + "' into field '" + testData.getFormField().getLabel() + "'");
					itemTestData.setImage(CodeCadenzaResourcePlugin.getImage(IMG_CLIENT_TEXT));
				}
			}
			else if (testData.getTableColumnField() != null) {
				final var itemTestData = new TreeItem(itemTestDataFolder, SWT.NONE);
				itemTestData.setImage(CodeCadenzaResourcePlugin.getImage(IMG_COLUMN));

				if (testData.getType() == GUITestDataType.SEARCH_FILTER)
					itemTestData.setText(
							"Use filter value '" + testData.getNewValue() + "' for column '" + testData.getTableColumnField().getTitle() + "'");
				else if (testData.getType() == GUITestDataType.SEARCH_OPERATOR)
					itemTestData.setText("Select search operator '" + testData.getNewValue() + "' for column '"
							+ testData.getTableColumnField().getTitle() + "'");
				else if (testData.getType() == GUITestDataType.SEARCH_SORT_ORDER)
					itemTestData.setText(
							"Use sort order '" + testData.getNewValue() + "' for column '" + testData.getTableColumnField().getTitle() + "'");
			}
			else {
				final var itemTestData = new TreeItem(itemTestDataFolder, SWT.NONE);

				if (testData.getType() == GUITestDataType.OBJECT_ID) {
					itemTestData.setText("Use object ID '" + testData.getFilterValue() + "'");
					itemTestData.setImage(CodeCadenzaResourcePlugin.getImage(IMG_KEY));
				}
				else if (testData.getType() == GUITestDataType.CELL_VALUE) {
					itemTestData.setText("Search for cell value '" + testData.getFilterValue() + "'");
					itemTestData.setImage(CodeCadenzaResourcePlugin.getImage(IMG_INDEX));
				}
				else if (testData.getType() == GUITestDataType.ROW_INDEX) {
					itemTestData.setText("Search for row index '" + testData.getFilterValue() + "'");
					itemTestData.setImage(CodeCadenzaResourcePlugin.getImage(IMG_INDEX));
				}
				else if (testData.getType() == GUITestDataType.ROW_COUNT) {
					itemTestData.setText("Row count '" + testData.getExpectedValue() + "'");
					itemTestData.setImage(CodeCadenzaResourcePlugin.getImage(IMG_INDEX));
				}
				else if (testData.getType() == GUITestDataType.PAGE_TITLE) {
					itemTestData.setText("Expected page title '" + testData.getExpectedValue() + "'");
					itemTestData.setImage(CodeCadenzaResourcePlugin.getImage(IMG_INDEX));
				}
				else if (testData.getType() == GUITestDataType.FORM_FIELD) {
					if (testAction.getFormAction() != null) {
						final ActionType type = testAction.getFormAction().getType();

						if (type == ActionType.DIRECT_UPLOAD || type == ActionType.INDIRECT_UPLOAD || type == ActionType.UPLOAD_IMPORT) {
							itemTestData.setText("Upload file '" + testData.getNewValue() + "'");
							itemTestData.setImage(CodeCadenzaResourcePlugin.getImage(IMG_UPLOAD));
						}
					}

					if (testAction.getType() == GUITestActionType.OPEN_LOGIN_PAGE) {
						final var fieldType = testAction.getTestData().indexOf(testData) == 0 ? "user name" : "password";

						itemTestData.setText("Enter '" + testData.getNewValue() + "' into " + fieldType + " field");
						itemTestData.setImage(CodeCadenzaResourcePlugin.getImage(IMG_CLIENT_TEXT));
					}
				}
			}
		});

		itemTestDataFolder.setExpanded(expandTreeItems);
	}

}
