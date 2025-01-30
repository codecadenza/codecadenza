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
package net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.exchange;

import static net.codecadenza.eclipse.shared.Constants.ACTION_PREFIX;
import static net.codecadenza.eclipse.shared.Constants.EL_I18N_VAR;

import java.util.List;
import java.util.regex.Pattern;
import net.codecadenza.eclipse.generator.client.common.service.ServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.util.JSFGeneratorUtil;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.client.ActionType;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormAction;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;

/**
 * <p>
 * Generator for data import operations
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JSFImportGenerator {
	private static final Pattern ACTION_PREFIX_PATTERN = Pattern.compile(ACTION_PREFIX);

	private final AbstractJavaSourceGenerator generator;
	private final String managedBeanName;
	private final List<FormAction> actions;

	/**
	 * Constructor
	 * @param generator
	 * @param form
	 */
	public JSFImportGenerator(AbstractJavaSourceGenerator generator, Form form) {
		this(generator, JSFGeneratorUtil.createManagedBeanName(form.getName()), form.getActions());
	}

	/**
	 * Constructor
	 * @param generator
	 * @param panel
	 */
	public JSFImportGenerator(AbstractJavaSourceGenerator generator, FormPanel panel) {
		this(generator, JSFGeneratorUtil.createManagedBeanName(panel.getName()), panel.getActions());
	}

	/**
	 * Constructor
	 * @param generator
	 * @param managedBeanName
	 * @param actions
	 */
	public JSFImportGenerator(AbstractJavaSourceGenerator generator, String managedBeanName, List<FormAction> actions) {
		this.generator = generator;
		this.managedBeanName = managedBeanName;
		this.actions = actions.stream().filter(e -> e.getType() == ActionType.UPLOAD_IMPORT).toList();
	}

	/**
	 * Add all necessary imports
	 */
	public void addImports() {
		actions.forEach(action -> {
			final var exchangeMethod = (DataExchangeMethod) action.getBoundaryMethod().getServiceMethod();
			final boolean processContent = action.getBoundaryMethod().getMethodParameters().size() == 1;

			if (processContent) {
				generator.importPackage("org.primefaces.event");

				if (exchangeMethod.hasPathParameter())
					generator.importPackage("java.io");
			}
		});
	}

	/**
	 * @return the generated content
	 */
	public String createDialogFragment() {
		final var b = new StringBuilder();

		for (final FormAction action : actions) {
			final var exchangeMethod = (DataExchangeMethod) action.getBoundaryMethod().getServiceMethod();

			// A dialog won't be necessary if an import operation just has to be started!
			if (exchangeMethod.getMethodParameters().isEmpty())
				continue;

			String actionName = action.getName();

			if (action.getName().startsWith(ACTION_PREFIX))
				actionName = ACTION_PREFIX_PATTERN.matcher(actionName).replaceFirst("");

			actionName = actionName.substring(0, 1).toUpperCase() + actionName.substring(1);

			final var widgetVar = "dlg" + actionName;
			final var dialogId = "dlg" + actionName + "Upload";
			final var controller = managedBeanName + ".on" + actionName;
			final var msgVar = "import" + actionName + "Msgs";

			// Set the default maximum file upload size
			final String maxFileSize = Integer.toString(Integer.MAX_VALUE);

			b.append("\t<p:dialog id=\"" + dialogId + "\" modal=\"true\" width=\"350\" height=\"250\" header=\"#{");
			b.append(EL_I18N_VAR + ".dialog_upload_title}\" widgetVar=\"" + widgetVar + "\">\n");
			b.append("\t\t<p:messages id=\"" + msgVar + "\" showDetail=\"true\" showSummary=\"true\"/>\n");
			b.append("\t\t<p:fileUpload auto=\"true\" mode=\"advanced\" listener=\"#{" + controller);
			b.append("}\" sizeLimit=\"" + maxFileSize + "\" update=\"" + msgVar + "\"/>\n");
			b.append("\t</p:dialog>\n\n");
		}

		return b.toString();
	}

	/**
	 * Add all data import methods
	 */
	public void addDataImportMethods() {
		actions.forEach(action -> {
			final var b = new StringBuilder();
			final var exchangeMethod = (DataExchangeMethod) action.getBoundaryMethod().getServiceMethod();
			final BoundaryMethod boundaryMethod = action.getBoundaryMethod();
			final boolean processContent = boundaryMethod.getMethodParameters().size() == 1;
			final var invocationGenerator = new ServiceInvocationGenerator(boundaryMethod, b);
			String methodName = boundaryMethod.getName();
			var methodSignature = "void " + methodName + "()";

			if (processContent) {
				methodName = action.getName();

				if (action.getName().startsWith(ACTION_PREFIX))
					methodName = ACTION_PREFIX_PATTERN.matcher(methodName).replaceFirst("");

				methodName = "on" + methodName.substring(0, 1).toUpperCase() + methodName.substring(1);
				methodSignature = "void " + methodName + "(FileUploadEvent event)";
			}

			b.append("/**\n");
			b.append(" * " + action.getDescription() + "\n");

			if (processContent)
				b.append(" * @param event\n");

			b.append(" */\n");
			b.append(generator.getAnnotationForGeneratedElement());
			b.append("public " + methodSignature + "\n");
			b.append("{\n");

			generator.addDebugLog(b, "Start data import");

			b.append("\n");

			if (processContent) {
				if (exchangeMethod.hasPathParameter()) {
					b.append("final File tempFile;\n");
					b.append("final String fileName = event.getFile().getFileName();\n\n");
					b.append("try\n");
					b.append("{\n");
					b.append("tempFile = File.createTempFile(fileName, Long.toString(System.currentTimeMillis()));\n");
					b.append("}\n");
					b.append("catch (final IOException e)\n");
					b.append("{\n");

					generator.addErrorLog(b, "Error while creating temporary file {}!", "e", "fileName");

					b.append("\n");
					b.append("MessageUtil.sendFacesMessage(bundle, FacesMessage.SEVERITY_ERROR, OPERATION_UPLOAD_FAIL, e);\n");
					b.append("return;\n");
					b.append("}\n\n");
					b.append("try(final var fout = new FileOutputStream(tempFile))\n");
					b.append("{\n");
					b.append("event.getFile().getInputStream().transferTo(fout);\n\n");

					invocationGenerator.addInvocation("tempFile.getAbsolutePath()");
				}
				else {
					generator.importClass("java.nio.charset.StandardCharsets");

					b.append("// Convert input stream into string\n");
					b.append("try(final var scanner = new Scanner(event.getFile().getInputStream(), ");
					b.append(exchangeMethod.getStandardCharset() + ").useDelimiter(\"\\\\A\"))\n");
					b.append("{\n");
					b.append("final String fileContent = scanner.hasNext() ? scanner.next() : \"\";\n\n");

					invocationGenerator.addInvocation("fileContent");
				}
			}
			else {
				b.append("try\n");
				b.append("{\n");

				invocationGenerator.addInvocation();
			}

			b.append("\n");
			b.append("MessageUtil.sendFacesMessage(bundle, FacesMessage.SEVERITY_INFO, OPERATION_IMPORT_OK);\n");
			b.append("}\n");
			b.append("catch (final Exception e)\n");
			b.append("{\n");

			generator.addErrorLog(b, "Error while performing data import operation!", "e");

			b.append("\n");
			b.append("MessageUtil.sendFacesMessage(bundle, FacesMessage.SEVERITY_ERROR, OPERATION_IMPORT_FAIL, e);\n");
			b.append("}\n");
			b.append("}\n\n");

			generator.addMethod(methodSignature, b.toString());
		});
	}

}
