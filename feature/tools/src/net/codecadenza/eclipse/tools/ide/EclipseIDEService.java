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
package net.codecadenza.eclipse.tools.ide;

import static net.codecadenza.eclipse.shared.Constants.APPLICATION_MODEL_NAME;
import static net.codecadenza.eclipse.shared.Constants.DIAGRAM_EDITOR_ID;
import static net.codecadenza.eclipse.shared.Constants.MIN_ATTRIBUTE_NAME_LENGTH;
import static net.codecadenza.eclipse.shared.Constants.MIN_CLASS_NAME_LENGTH;
import static net.codecadenza.eclipse.shared.Constants.MODEL_FOLDER;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.regex.Pattern;
import java.util.stream.Stream;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.WorkspaceFile;
import net.codecadenza.eclipse.shared.Constants;
import net.codecadenza.eclipse.tools.CodeCadenzaToolsPlugin;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.xmi.XMLResource;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.core.JavaConventions;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.ToolFactory;
import org.eclipse.jdt.core.formatter.CodeFormatter;
import org.eclipse.jdt.core.manipulation.CodeGeneration;
import org.eclipse.jdt.internal.ui.JavaPlugin;
import org.eclipse.jdt.ui.JavaUI;
import org.eclipse.jdt.ui.refactoring.RenameSupport;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.templates.Template;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.text.edits.TextEdit;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;

/**
 * <p>
 * Class that provides IDE-related helper methods
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
@SuppressWarnings("restriction")
public class EclipseIDEService {
	private static final String JAVA_VERSION = JavaCore.VERSION_21;
	private static final String TEMPLATE_ID_NEW_TYPE = "org.eclipse.jdt.ui.text.codetemplates.newtype";
	private static final Pattern LINE_PATTERN = Pattern.compile("\\s*\\*.*");
	private static final Pattern COMMENT_PATTERN = Pattern.compile("\\s*//.*");

	/**
	 * Prevent instantiation
	 */
	private EclipseIDEService() {

	}

	/**
	 * @param name
	 * @return the default column name
	 */
	public static String buildDefaultColumnName(String name) {
		final var b = new StringBuilder();

		for (final char c : name.toCharArray())
			if (c == Character.toUpperCase(c))
				b.append("_" + Character.toLowerCase(c));
			else
				b.append(c);

		return b.toString();
	}

	/**
	 * @param attr
	 * @return the default column name
	 */
	public static String buildDefaultColumnName(DomainAttribute attr) {
		return buildDefaultColumnName(attr.getName());
	}

	/**
	 * @param assoc
	 * @return the default column name
	 */
	public static String buildDefaultColumnName(AbstractDomainAssociation assoc) {
		return buildDefaultColumnName(assoc.getName());
	}

	/**
	 * @param name
	 * @return the default label
	 */
	public static String buildDefaultLabel(String name) {
		final var b = new StringBuilder();

		for (final char c : name.toCharArray()) {
			if (c == Character.toUpperCase(c))
				b.append(" " + Character.toLowerCase(c));
			else
				b.append(c);
		}

		return b.toString().trim();
	}

	/**
	 * @param name
	 * @return the default plural label
	 */
	public static String buildDefaultPluralLabel(String name) {
		final String label = buildDefaultLabel(name);

		return buildDefaultPluralForm(label);
	}

	/**
	 * @param name
	 * @return the created plural form of the given name
	 */
	public static String buildDefaultPluralForm(String name) {
		if (name.endsWith("s"))
			return name + "es";
		else if (name.endsWith("y"))
			return name.substring(0, name.length() - 1) + "ies";
		else
			return name + "s";
	}

	/**
	 * @param label
	 * @return the created default comment for a domain object class
	 */
	public static String buildDomainObjectComment(String label) {
		return "Domain object for " + label + " objects";
	}

	/**
	 * Check if there is any open domain object diagram editor. This method has to be called always before an operation may change
	 * meta-model data! In previous solutions only dirty editors were checked. But this doesn't prevent synchronization problems.
	 * @param view
	 * @return true if there is at least one open editor
	 */
	public static boolean openDiagramExists(IViewPart view) {
		final Shell shell = view.getViewSite().getShell();
		final var msgTitle = "Possible resource conflict";

		for (final IEditorReference editorRef : view.getViewSite().getPage().getEditorReferences())
			if (editorRef.getId().equals(DIAGRAM_EDITOR_ID)) {
				MessageDialog.openInformation(shell, msgTitle,
						"Please close CodeCadenza diagram editor(s) in order to prevent resource problems!");
				return true;
			}

		return false;
	}

	/**
	 * Rename a project
	 * @param projectName the name of the project to be renamed
	 * @param newName the new name of the project
	 * @throws Exception if the rename operation has failed
	 */
	public static void renameProject(String projectName, String newName) throws Exception {
		final IWorkspaceRoot wsRoot = ResourcesPlugin.getWorkspace().getRoot();
		final IProject project = wsRoot.getProject(projectName);
		final IJavaProject javaProject = JavaCore.create(project);
		final Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();

		final RenameSupport refactor = RenameSupport.create(javaProject, newName,
				RenameSupport.UPDATE_REFERENCES | RenameSupport.UPDATE_TEXTUAL_MATCHES);
		refactor.openDialog(shell);
	}

	/**
	 * Rename a package
	 * @param projectName the name of the project
	 * @param sourceFolder the relative path of the source folder
	 * @param packageName the name of the package to be renamed
	 * @param newName the new name of the package
	 * @throws Exception if the rename operation has failed
	 */
	public static void renamePackage(String projectName, String sourceFolder, String packageName, String newName) throws Exception {
		final IWorkspaceRoot wsRoot = ResourcesPlugin.getWorkspace().getRoot();
		final IProject project = wsRoot.getProject(projectName);
		final IFolder folder = project.getFolder(sourceFolder);
		final IJavaProject javaProject = JavaCore.create(project);
		final IPackageFragmentRoot fragmentRoot = javaProject.getPackageFragmentRoot(folder);
		final IPackageFragment fragment = fragmentRoot.getPackageFragment(packageName);
		final Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();

		if (!fragment.exists())
			return;

		final RenameSupport refactor = RenameSupport.create(fragment, newName,
				RenameSupport.UPDATE_REFERENCES | RenameSupport.UPDATE_TEXTUAL_MATCHES);
		refactor.openDialog(shell);
	}

	/**
	 * Create a new package
	 * @param projectName the name of the project
	 * @param sourceFolder the relative path of the source folder
	 * @param packageName the fully qualified name of the package to be created
	 * @throws Exception if the package creation has failed
	 */
	public static void createPackage(String projectName, String sourceFolder, String packageName) throws Exception {
		final IWorkspaceRoot wsRoot = ResourcesPlugin.getWorkspace().getRoot();
		final IProject project = wsRoot.getProject(projectName);
		final IFolder folder = project.getFolder(sourceFolder);
		final IJavaProject javaProject = JavaCore.create(project);

		final IPackageFragmentRoot fragmentRoot = javaProject.getPackageFragmentRoot(folder);
		fragmentRoot.createPackageFragment(packageName, true, new NullProgressMonitor());
	}

	/**
	 * Rename a compilation unit
	 * @param javaFile
	 * @param newName the new name
	 * @throws Exception if the operation has failed
	 */
	public static void renameCompUnit(JavaFile javaFile, String newName) throws Exception {
		final IWorkspaceRoot wsRoot = ResourcesPlugin.getWorkspace().getRoot();
		final IProject project = wsRoot.getProject(javaFile.getProjectName());
		final IFolder folder = project.getFolder(javaFile.getSourceFolder());
		final IJavaProject javaProject = JavaCore.create(project);
		final IPackageFragmentRoot fragmentRoot = javaProject.getPackageFragmentRoot(folder);
		final IPackageFragment fragment = fragmentRoot.getPackageFragment(javaFile.getPackageName());
		final ICompilationUnit compUnit = fragment.getCompilationUnit(javaFile.getName());
		final Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();

		if (!compUnit.exists())
			return;

		final RenameSupport refactor = RenameSupport.create(compUnit, newName,
				RenameSupport.UPDATE_REFERENCES | RenameSupport.UPDATE_TEXTUAL_MATCHES);
		refactor.openDialog(shell);
	}

	/**
	 * Create a file within the workspace or update its content if the file already exists
	 * @param workspaceFile
	 * @return the created workspace file
	 * @throws Exception if either the create or the update operation has failed
	 */
	public static IFile createOrUpdateFile(WorkspaceFile workspaceFile) throws Exception {
		final IWorkspaceRoot wsRoot = ResourcesPlugin.getWorkspace().getRoot();
		final IProject project = wsRoot.getProject(workspaceFile.getProjectName());
		final IFile file = project.getFile(workspaceFile.getPath());

		if (!file.exists())
			file.create(new ByteArrayInputStream(workspaceFile.getContent().getBytes()), true, null);
		else
			file.setContents(new ByteArrayInputStream(workspaceFile.getContent().getBytes()), true, false, null);

		return file;
	}

	/**
	 * Create a folder in the given project's workspace if it doesn't exist
	 * @param projectName
	 * @param folderName
	 * @return the created workspace folder
	 * @throws Exception if the create operation has failed
	 */
	public static IFolder createFolder(String projectName, String folderName) throws Exception {
		final IWorkspaceRoot wsRoot = ResourcesPlugin.getWorkspace().getRoot();
		final IProject project = wsRoot.getProject(projectName);
		final IFolder folder = project.getFolder(folderName);
		final var folderStack = new LinkedList<IFolder>();
		IContainer parentContainer = folder.getParent();

		// Iterate over all parent folders and add them on top of the internal stack
		while (parentContainer != null) {
			if (!(parentContainer instanceof final IFolder parentFolder))
				break;

			folderStack.push(parentFolder);

			parentContainer = parentContainer.getParent();
		}

		// Iterate over all folders in the stack and create every folder
		while (!folderStack.isEmpty()) {
			final IFolder parentFolder = folderStack.pop();

			if (!parentFolder.exists())
				parentFolder.create(true, true, new NullProgressMonitor());
		}

		if (!folder.exists())
			folder.create(true, true, new NullProgressMonitor());

		return folder;
	}

	/**
	 * Overwrite the content of the existing file
	 * @param projectName
	 * @param fileName
	 * @param content
	 * @throws Exception if the write operation has failed
	 */
	public static void overwriteFileContent(String projectName, String fileName, String content) throws Exception {
		final IWorkspaceRoot wsRoot = ResourcesPlugin.getWorkspace().getRoot();
		final IProject project = wsRoot.getProject(projectName);
		final IFile file = project.getFile(fileName);

		file.setContents(new ByteArrayInputStream(content.getBytes()), true, false, null);
	}

	/**
	 * Add a view to the application model
	 * @param projectName the name of the project
	 * @param viewName the name of the view
	 * @param className the fully qualified class name
	 * @throws Exception if adding of the view has failed
	 */
	public static void addViewToApplicationModel(String projectName, String viewName, String className) throws Exception {
		// Get the content of the application model file and add a new view element
		final String result = new E4ApplicationXMLEditor(getApplicationModelContent(projectName)).addView(className, viewName);

		if (!result.isEmpty())
			saveApplicationModel(projectName, result);
	}

	/**
	 * Remove a view from the application model
	 * @param projectName the name of the project
	 * @param className the fully qualified class name
	 * @throws Exception if the removing of the view has failed
	 */
	public static void removeViewFromApplicationModel(String projectName, String className) throws Exception {
		// Get the content of the application model file and remove the view
		final String result = new E4ApplicationXMLEditor(getApplicationModelContent(projectName)).removeView(className);

		if (!result.isEmpty())
			saveApplicationModel(projectName, result);
	}

	/**
	 * Save the application model
	 * @param projectName
	 * @param content
	 * @throws Exception if the save operation has failed
	 */
	public static void saveApplicationModel(String projectName, String content) throws Exception {
		final IWorkspaceRoot wsRoot = ResourcesPlugin.getWorkspace().getRoot();
		final IProject project = wsRoot.getProject(projectName);
		final IFile file = project.getFile(APPLICATION_MODEL_NAME);

		file.setContents(new ByteArrayInputStream(content.getBytes()), true, false, null);
	}

	/**
	 * Delete a file in the workspace
	 * @param projectName
	 * @param path
	 * @throws Exception if the delete operation has failed
	 */
	public static void deleteWorkspaceFile(String projectName, String path) throws Exception {
		final IWorkspaceRoot wsRoot = ResourcesPlugin.getWorkspace().getRoot();
		final IProject project = wsRoot.getProject(projectName);
		final IFile file = project.getFile(path);

		if (file.exists())
			file.delete(true, null);
	}

	/**
	 * Delete a file in the workspace
	 * @param workspaceFile
	 * @throws Exception if the delete operation has failed
	 */
	public static void deleteWorkspaceFile(WorkspaceFile workspaceFile) throws Exception {
		deleteWorkspaceFile(workspaceFile.getProjectName(), workspaceFile.getPath());
	}

	/**
	 * Get the content of the project's application model file
	 * @param projectName
	 * @return the content of the plugin.xml file
	 * @throws Exception if the reading of the content has failed
	 */
	public static String getApplicationModelContent(String projectName) throws Exception {
		final var b = new StringBuilder();

		final List<String> lines = getFileContent(projectName, APPLICATION_MODEL_NAME);
		lines.forEach(b::append);

		return b.toString();
	}

	/**
	 * Get the content of a file as a list with an entry for each line
	 * @param projectName
	 * @param filePath
	 * @return a list with all lines
	 * @throws Exception if the reading of the content has failed
	 */
	public static List<String> getFileContent(String projectName, String filePath) throws Exception {
		final IWorkspaceRoot wsRoot = ResourcesPlugin.getWorkspace().getRoot();
		final IProject project = wsRoot.getProject(projectName);
		final IFile file = project.getFile(filePath);
		final InputStream stream = file.getContents();
		final var lines = new ArrayList<String>();
		String line = null;

		try (final var reader = new BufferedReader(new InputStreamReader(stream))) {
			while ((line = reader.readLine()) != null)
				lines.add(line + "\n");
		}

		return lines;
	}

	/**
	 * This method should solve the problem of using the ${typecomment} in different contexts. It might be the case that this
	 * variable occurs within a comment or not. Depending on that the method has to modify the type comment appropriately in order
	 * to prevent build or compilation errors.
	 * @param comment
	 * @return the type comment or null if a template either doesn't exist, or the template doesn't contain the ${typecomment}
	 *         variable
	 */
	private static String prepareTypeComment(String comment) {
		final var typeComment = new StringBuilder();

		// First we search for the appropriate template
		final Template template = JavaPlugin.getDefault().getCodeTemplateStore().findTemplateById(TEMPLATE_ID_NEW_TYPE);

		if (template == null || template.getPattern().isEmpty())
			return null;

		if (comment == null || comment.isEmpty())
			return null;

		// The template exists. Now we search for the type comment variable
		final String pattern = template.getPattern();

		for (final String line : pattern.split("\n")) {
			if (line.contains("${typecomment}")) {
				// We check if the line resides within a comment
				if (LINE_PATTERN.matcher(line).matches()) {
					// If a line starts with an asterisk we assume that there is a valid Javadoc comment!
					boolean firstLine = true;

					for (final String commentLine : comment.split("\n")) {
						if (firstLine) {
							typeComment.append(commentLine);
							firstLine = false;
						}
						else
							typeComment.append("\n * " + commentLine);
					}
				}
				else if (COMMENT_PATTERN.matcher(line).matches()) {
					boolean firstLine = true;

					for (final String commentLine : comment.split("\n")) {
						if (firstLine) {
							typeComment.append(commentLine);
							firstLine = false;
						}
						else
							typeComment.append("\n// " + commentLine);
					}
				}
				else {
					// The type comment must be surrounded with comment characters
					typeComment.append("/*\n");

					for (final String commentLine : comment.split("\n"))
						typeComment.append(" * " + commentLine + "\n");

					typeComment.append(" */\n");
				}

				return typeComment.toString();
			}
		}

		// No type comment variable is defined in this template so we skip the comment completely
		return null;
	}

	/**
	 * Create a new Java file or rebuild it if it already exists
	 * @param javaFile
	 * @throws Exception if the file create operation has failed
	 */
	public static void createJavaFile(final JavaFile javaFile) throws Exception {
		final IWorkspaceRoot wsRoot = ResourcesPlugin.getWorkspace().getRoot();
		final IProject project = wsRoot.getProject(javaFile.getProjectName());
		final IFolder folder = project.getFolder(javaFile.getPath());
		final IJavaProject javaProject = JavaCore.create(project);
		final IPackageFragmentRoot fragmentRoot = javaProject.getPackageFragmentRoot(folder);
		final IPackageFragment fragment = fragmentRoot.getPackageFragment(javaFile.getPackageName());

		// Get the compilation unit
		ICompilationUnit compUnit = fragment.getCompilationUnit(javaFile.getName());

		if (!compUnit.exists())
			compUnit = fragment.createCompilationUnit(javaFile.getName(), javaFile.getContent(), true, new NullProgressMonitor());
		else
			compUnit = fragment.getCompilationUnit(javaFile.getName());

		final String typeComment = prepareTypeComment(javaFile.getComment());
		final String sourceInclTemplate = CodeGeneration.getCompilationUnitContent(compUnit, typeComment, javaFile.getContent(),
				"\n");
		final CodeFormatter formatter = ToolFactory.createCodeFormatter(javaProject.getOptions(true));
		final var doc = new Document(sourceInclTemplate);

		synchronized (formatter) {
			try {
				final TextEdit edit = formatter.format(CodeFormatter.K_COMPILATION_UNIT, sourceInclTemplate, 0,
						sourceInclTemplate.length() - 1, 0, "\n");

				if (edit != null)
					edit.apply(doc, TextEdit.NONE);
			}
			catch (final Exception e) {
				CodeCadenzaToolsPlugin.getInstance().logInfo(null, e);
			}
		}

		// Save the contents to the compilation unit
		compUnit.getBuffer().setContents(doc.get());
		compUnit.save(new NullProgressMonitor(), true);
		compUnit.close();
	}

	/**
	 * Open the given Java source file in the default editor of the IDE
	 * @param javaFile
	 * @throws Exception if the opening of the given file has failed!
	 */
	public static void openInEditor(JavaFile javaFile) throws Exception {
		final String resourceName = javaFile.getName();
		final IWorkspaceRoot wsRoot = ResourcesPlugin.getWorkspace().getRoot();
		final IProject project = wsRoot.getProject(javaFile.getProjectName());
		final IFolder folder = project.getFolder(javaFile.getSourceFolder());
		final IJavaProject javaProject = JavaCore.create(project);
		final IPackageFragmentRoot fragmentRoot = javaProject.getPackageFragmentRoot(folder);
		final IPackageFragment fragment = fragmentRoot.getPackageFragment(javaFile.getPackageName());
		final ICompilationUnit compUnit = fragment.getCompilationUnit(resourceName);

		JavaUI.openInEditor(compUnit);
	}

	/**
	 * Open the given workspace file in the default editor of the IDE
	 * @param workspaceFile
	 * @return an open editor or null if an external editor was opened or if opening was canceled
	 * @throws FileNotFoundException if the file could not be found
	 * @throws Exception if the opening of the given file has failed!
	 */
	public static IEditorPart openInEditor(WorkspaceFile workspaceFile) throws Exception {
		final IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
		final IWorkbenchPage page = window.getActivePage();
		final IWorkspaceRoot wsRoot = ResourcesPlugin.getWorkspace().getRoot();
		final IProject project = wsRoot.getProject(workspaceFile.getProjectName());
		final IFile file = project.getFile(workspaceFile.getPath());

		if (!file.exists())
			throw new FileNotFoundException("The file '" + file.getName() + "' could not be found!");

		return IDE.openEditor(page, file, true);
	}

	/**
	 * Delete the given Java source file in the workspace
	 * @param javaFile
	 * @throws Exception if the delete operation has failed
	 */
	public static void deleteSource(JavaFile javaFile) throws Exception {
		final String resourceName = javaFile.getName();
		final IWorkspaceRoot wsRoot = ResourcesPlugin.getWorkspace().getRoot();
		final IProject project = wsRoot.getProject(javaFile.getProjectName());
		final IFolder folder = project.getFolder(javaFile.getSourceFolder());
		final IJavaProject javaProject = JavaCore.create(project);
		final IPackageFragmentRoot fragmentRoot = javaProject.getPackageFragmentRoot(folder);
		final IPackageFragment fragment = fragmentRoot.getPackageFragment(javaFile.getPackageName());
		final ICompilationUnit compUnit = fragment.getCompilationUnit(resourceName);

		if (compUnit.exists())
			compUnit.delete(true, null);
	}

	/**
	 * Save the project meta-data and refresh the meta-data folder
	 * @param project
	 * @throws Exception if either the refresh or the save operation has failed
	 */
	public static void saveProjectMetaData(Project project) throws Exception {
		saveProjectMetaData(project.eResource(), project);
	}

	/**
	 * Save the project meta-data and refresh the meta-data folder
	 * @param resource
	 * @param project
	 * @throws Exception if either the refresh or the save operation has failed
	 */
	public static void saveProjectMetaData(Resource resource, Project project) throws Exception {
		final IWorkspaceRoot wsRoot = ResourcesPlugin.getWorkspace().getRoot();
		final IProject proj = wsRoot.getProject(project.getTargetProjectName(BuildArtifactType.DOMAIN));

		final var options = new HashMap<String, String>();
		options.put(XMLResource.OPTION_PROCESS_DANGLING_HREF, "DISCARD");

		resource.save(options);

		final IFolder folder = proj.getFolder(MODEL_FOLDER);
		folder.refreshLocal(IResource.DEPTH_ONE, null);
	}

	/**
	 * Check if the provided string represents a valid Java type name
	 * @param typeName
	 * @return the validation result
	 */
	public static IStatus validateJavaTypeName(String typeName) {
		final IStatus status = JavaConventions.validateJavaTypeName(typeName, JAVA_VERSION, JAVA_VERSION, null);

		if (status.getSeverity() > Status.INFO)
			return status;

		if (typeName.length() < MIN_CLASS_NAME_LENGTH) {
			final var msg = "The name is too short! The min. length is " + MIN_CLASS_NAME_LENGTH + " characters!";
			return new Status(IStatus.ERROR, CodeCadenzaToolsPlugin.PLUGIN_ID, IStatus.ERROR, msg, null);
		}

		return Status.OK_STATUS;
	}

	/**
	 * Check if the provided string represents a valid Java field name
	 * @param fieldName
	 * @return the validation result
	 */
	public static IStatus validateFieldName(String fieldName) {
		final IStatus status = JavaConventions.validateFieldName(fieldName, JAVA_VERSION, JAVA_VERSION);

		if (status.getSeverity() > Status.INFO)
			return status;

		if (fieldName.length() < MIN_ATTRIBUTE_NAME_LENGTH) {
			final var msg = "The name is too short! The min. length is " + MIN_ATTRIBUTE_NAME_LENGTH + " characters!";
			return new Status(IStatus.ERROR, CodeCadenzaToolsPlugin.PLUGIN_ID, IStatus.ERROR, msg, null);
		}

		return Status.OK_STATUS;
	}

	/**
	 * Check if the provided string represents a valid Java method name
	 * @param methodName
	 * @return the validation result
	 */
	public static IStatus validateMethodName(String methodName) {
		return JavaConventions.validateMethodName(methodName, JAVA_VERSION, JAVA_VERSION);
	}

	/**
	 * Check if the provided string represents a valid Java package name
	 * @param packageName
	 * @return the validation result
	 */
	public static IStatus validatePackageName(String packageName) {
		return JavaConventions.validatePackageName(packageName, JAVA_VERSION, JAVA_VERSION);
	}

	/**
	 * Check if the provided string represents a valid Java type variable name
	 * @param variableName
	 * @return the validation result
	 */
	public static IStatus validateTypeVariableName(String variableName) {
		return JavaConventions.validateTypeVariableName(variableName, JAVA_VERSION, JAVA_VERSION);
	}

	/**
	 * Check if the Eclipse workbench contains the M2E plug-in
	 * @return true if the respective plug-in is installed
	 */
	public static boolean isM2EInstalled() {
		final IWorkspace workspace = ResourcesPlugin.getWorkspace();

		// We assume that the M2E plug-in is installed if the selected project nature exists!
		final IStatus status = workspace.validateNatureSet(Stream.of(Constants.MAVEN_NATURE_ID).toArray(size -> new String[size]));

		return status.getCode() == IStatus.OK;
	}

	/**
	 * Create a new project in the workspace
	 * @param root
	 * @param projectDescription
	 * @param encoding
	 * @param monitor
	 * @return the created project
	 * @throws Exception if the creation of a new workspace project has failed
	 */
	public static IProject createNewProject(IWorkspaceRoot root, IProjectDescription projectDescription, String encoding,
			IProgressMonitor monitor) throws Exception {
		final IProject project = root.getProject(projectDescription.getName());

		if (!project.exists())
			project.create(projectDescription, monitor);

		if (!project.isOpen())
			project.open(monitor);

		if (encoding != null)
			project.setDefaultCharset(encoding, monitor);

		return project;
	}

}
