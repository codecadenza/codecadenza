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
package net.codecadenza.eclipse.generator.client.imp.angular.util;

import static net.codecadenza.eclipse.shared.Constants.ANGULAR_COMMON_SERVICES_FOLDER;
import static net.codecadenza.eclipse.shared.Constants.TRANSLATION_FILE_NAME;

import net.codecadenza.eclipse.generator.CodeCadenzaGeneratorPlugin;
import net.codecadenza.eclipse.generator.client.common.i18n.AbstractI18NGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.common.AngularContentFormatter;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.WorkspaceFile;
import net.codecadenza.eclipse.shared.Constants;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;

/**
 * <p>
 * Generator for internationalization code
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class AngularI18NGenerator extends AbstractI18NGenerator {
	/**
	 * Constructor
	 * @param project
	 */
	public AngularI18NGenerator(Project project) {
		super(project);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.i18n.AbstractI18NGenerator#getTranslationFilePath()
	 */
	@Override
	public String getTranslationFilePath() {
		return Constants.ANGULAR_LOCALE_FOLDER + "/" + TRANSLATION_FILE_NAME;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.i18n.AbstractI18NGenerator#getCodeFragment(java.lang.String)
	 */
	@Override
	public String getCodeFragment(String key) {
		return "this.i18n.translate('" + key + "')";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.i18n.AbstractI18NGenerator#getCodeFragmentWithParam(java.lang.String,
	 * java.lang.String)
	 */
	@Override
	public String getCodeFragmentWithParam(String key, String param) {
		return "this.i18n.translate('" + key + "', " + param + ")";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.i18n.AbstractI18NGenerator#getCodeFragmentForFieldLabel(java.lang.String)
	 */
	@Override
	public String getCodeFragmentForFieldLabel(String key) {
		throw new UnsupportedOperationException(
				"The method getCodeFragmentForFieldLabel() hasn't been implemented for Angular generators!");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.i18n.AbstractI18NGenerator#save()
	 */
	@Override
	public void save() {
		super.save();

		try {
			createI18NService();
		}
		catch (final Exception e) {
			CodeCadenzaGeneratorPlugin.getInstance().logError(e);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.i18n.AbstractI18NGenerator#createI18NService()
	 */
	@Override
	public void createI18NService() throws Exception {
		final var methodComment = new StringBuilder();
		methodComment.append("Return the translation messages for a given locale.\n");
		methodComment.append("It will return an empty array if the locale is not supported!");

		final var formatter = new AngularContentFormatter();
		formatter.addLine("import { Translation } from '../model/translation.interface';");
		formatter.addBlankLine();
		formatter.addBlockComment("This class contains all translations that are used in TypeScript files");
		formatter.addLine("export class TranslationMessageRepository {");
		formatter.increaseIndent();
		formatter.addLine("static readonly LOCALE_EN = 'EN';");
		formatter.addBlankLine();
		formatter.addLineComment("Translation messages for the default locale");
		formatter.addLine("private static readonly messagesEN: Array<Translation> =");
		formatter.increaseIndent();
		formatter.addLine("[");
		formatter.increaseIndent();

		propertiesEN.keySet().forEach(
				key -> formatter.addLine("{ 'key': '" + key + "', 'message': '" + propertiesEN.getProperty((String) key) + "' },"));

		formatter.decreaseIndent();
		formatter.addLine("];");
		formatter.decreaseIndent();
		formatter.addBlankLine();
		formatter.addBlockComment(methodComment.toString());
		formatter.addLine("static getTranlationMessages(locale: string) {");
		formatter.increaseIndent();
		formatter.addIfStatement("locale === TranslationMessageRepository.LOCALE_EN",
				"return TranslationMessageRepository.messagesEN;", true);
		formatter.addLine("console.warn('No messages found for locale ' + locale + '!');");
		formatter.addLine("return [];");
		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.decreaseIndent();
		formatter.addLine("}");

		final var path = ANGULAR_COMMON_SERVICES_FOLDER + "/translation-message-repository.ts";
		final var sourceFile = new WorkspaceFile(project, BuildArtifactType.GUI, path, formatter.getContent());

		EclipseIDEService.createOrUpdateFile(sourceFile);
	}

}
