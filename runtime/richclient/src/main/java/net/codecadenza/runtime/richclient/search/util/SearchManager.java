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
package net.codecadenza.runtime.richclient.search.util;

import java.text.DecimalFormatSymbols;
import java.util.Collection;
import java.util.List;
import net.codecadenza.runtime.richclient.persistence.PersistenceHelper;
import net.codecadenza.runtime.richclient.search.event.SearchDTOChangeController;
import net.codecadenza.runtime.richclient.transport.ServiceLocator;
import net.codecadenza.runtime.search.dto.SearchDTO;
import net.codecadenza.runtime.search.dto.SearchFieldDTO;
import net.codecadenza.runtime.search.dto.SearchListDTO;

/**
 * <p>
 * Utility class to handle persistent search objects
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SearchManager {
	/**
	 * Prevent instantiation
	 */
	private SearchManager() {

	}

	/**
	 * @param sourceObject
	 * @return a clone of the given search object
	 */
	private static SearchDTO createClone(SearchDTO sourceObject) {
		final var clone = new SearchDTO();

		clone.setId(sourceObject.getId());
		clone.setCaseSensitive(sourceObject.isCaseSensitive());
		clone.setCount(sourceObject.isCount());
		clone.setDateFormat(sourceObject.getDateFormat());
		clone.setDateTimeFormat(sourceObject.getDateTimeFormat());
		clone.setExactFilterMatch(sourceObject.isExactFilterMatch());
		clone.setFetchHidden(sourceObject.isFetchHidden());
		clone.setFromClause(sourceObject.getFromClause());
		clone.setGroupBy(sourceObject.getGroupBy());
		clone.setMaxResult(sourceObject.getMaxResult());
		clone.setNumberFormat(sourceObject.getNumberFormat());
		clone.setStartIndex(sourceObject.getStartIndex());
		clone.setDecimalSeparator(DecimalFormatSymbols.getInstance().getDecimalSeparator());
		clone.setGroupingSeparator(DecimalFormatSymbols.getInstance().getGroupingSeparator());

		sourceObject.getSearchFields().forEach(sourceField -> {
			final var cloneField = new SearchFieldDTO();

			cloneField.setColOrder(sourceField.getColOrder());
			cloneField.setColumnIndex(sourceField.getColumnIndex());
			cloneField.setColLabel(sourceField.getColLabel());
			cloneField.setColName(sourceField.getColName());
			cloneField.setColWidth(sourceField.getColWidth());
			cloneField.setDataType(sourceField.getDataType());
			cloneField.setDateTimeFormat(sourceField.isDateTimeFormat());
			cloneField.setEnumListValues(sourceField.getEnumListValues());
			cloneField.setFetchIndex(sourceField.getFetchIndex());
			cloneField.setFilterCriteria(sourceField.getFilterCriteria());
			cloneField.setListOfValues(sourceField.getListOfValues());
			cloneField.setLovCommand(sourceField.getLovCommand());
			cloneField.setOperator(sourceField.getOperator());
			cloneField.setOriginalColumnIndex(sourceField.getOriginalColumnIndex());
			cloneField.setSortIndex(sourceField.getSortIndex());
			cloneField.setSortOrder(sourceField.getSortOrder());
			cloneField.setType(sourceField.getType());
			cloneField.setVisible(sourceField.isVisible());

			clone.getSearchFields().add(cloneField);
		});

		return clone;
	}

	/**
	 * Get all saved queries
	 * @return a collection of saved queries
	 */
	public static synchronized Collection<SearchListDTO> getAllSavedSearchObjects() {
		return PersistenceHelper.findPersistentObjects(SearchListDTO.class).stream()
				.filter(listEntry -> listEntry.getUserName() != null && listEntry.getName() != null
						&& listEntry.getUserName().equals(ServiceLocator.getUserName()) && !listEntry.getName().isEmpty())
				.toList();
	}

	/**
	 * Overwrite an existing saved query
	 * @param id the id of the existing query
	 * @param searchDTO the search object with the new settings to be saved
	 */
	public static synchronized void overwriteSavedSearchObject(int id, SearchDTO searchDTO) {
		final SearchDTO dto = PersistenceHelper.findPersistentObjects(SearchDTO.class).stream()
				.filter(searchObj -> searchObj.getId() == id).findFirst().orElse(null);

		if (dto == null)
			return;

		dto.setCaseSensitive(searchDTO.isCaseSensitive());
		dto.setCount(searchDTO.isCount());
		dto.setDateFormat(searchDTO.getDateFormat());
		dto.setDateTimeFormat(searchDTO.getDateTimeFormat());
		dto.setExactFilterMatch(searchDTO.isExactFilterMatch());
		dto.setFetchHidden(searchDTO.isFetchHidden());
		dto.setFromClause(searchDTO.getFromClause());
		dto.setGroupBy(searchDTO.getGroupBy());
		dto.setMaxResult(searchDTO.getMaxResult());
		dto.setNumberFormat(searchDTO.getNumberFormat());
		dto.setStartIndex(searchDTO.getStartIndex());

		for (final SearchFieldDTO newField : searchDTO.getSearchFields())
			dto.getSearchFields().stream().filter(oldField -> newField.getColName().equals(oldField.getColName())).forEach(oldField -> {
				oldField.setColOrder(newField.getColOrder());
				oldField.setColumnIndex(newField.getColumnIndex());
				oldField.setColWidth(newField.getColWidth());
				oldField.setFetchIndex(newField.getFetchIndex());
				oldField.setFilterCriteria(newField.getFilterCriteria());
				oldField.setOperator(newField.getOperator());
				oldField.setOriginalColumnIndex(newField.getOriginalColumnIndex());
				oldField.setSortIndex(newField.getSortIndex());
				oldField.setSortOrder(newField.getSortOrder());
				oldField.setVisible(newField.isVisible());
			});

		PersistenceHelper.save();
	}

	/**
	 * Delete the saved query
	 * @param id
	 */
	public static synchronized void deleteSavedSearchObject(int id) {
		final List<SearchDTO> list = PersistenceHelper.findPersistentObjects(SearchDTO.class);

		for (final SearchDTO searchObj : list)
			if (searchObj.getId() == id) {
				PersistenceHelper.removePersistentObject(searchObj);
				break;
			}

		final List<SearchListDTO> searchList = PersistenceHelper.findPersistentObjects(SearchListDTO.class);

		for (final SearchListDTO listEntry : searchList)
			if (listEntry.getId() == id) {
				PersistenceHelper.removePersistentObject(listEntry);
				break;
			}

		PersistenceHelper.save();
	}

	/**
	 * Get a saved search
	 * @param id the id of the search object
	 * @return the persistent search object
	 */
	public static synchronized SearchDTO getSavedSearch(int id) {
		return PersistenceHelper.findPersistentObjects(SearchDTO.class).stream().filter(dto -> dto.getId() == id)
				.map(SearchManager::createClone).findFirst().orElse(null);
	}

	/**
	 * @return the next ID for a search object
	 */
	private static synchronized int getNextId() {
		return PersistenceHelper.findPersistentObjects(SearchListDTO.class).stream().map(SearchListDTO::getId).max(Integer::compare)
				.orElse(0) + 1;
	}

	/**
	 * Save the query
	 * @param viewName the name of the view
	 * @param searchDTO the search data transfer object to be saved
	 * @param name the display name
	 * @throws DuplicateSearchNameException if a saved search with the same name already exists
	 */
	public static synchronized void saveSearch(String viewName, SearchDTO searchDTO, String name)
			throws DuplicateSearchNameException {
		// Check if the user has already saved a query with the same name
		final List<SearchListDTO> list = PersistenceHelper.findPersistentObjects(SearchListDTO.class);

		final var newElement = new SearchListDTO(-1, name, viewName);
		newElement.setUserName(ServiceLocator.getUserName());

		for (final SearchListDTO listEntry : list)
			if (listEntry.equals(newElement))
				throw new DuplicateSearchNameException();

		// Get the next ID
		final int id = getNextId();
		newElement.setId(id);

		PersistenceHelper.addPersistentObject(newElement);

		searchDTO.setId(id);

		// Create a clone of the given search object in order to save its current state properly!
		PersistenceHelper.addPersistentObject(createClone(searchDTO));
		PersistenceHelper.save();

		// Notify the event controller about a new saved query!
		SearchDTOChangeController.fireNewSearchDTO(newElement);
	}

	/**
	 * Save the last search
	 * @param viewName the name of the view
	 * @param searchDTO the search data transfer object to be saved
	 */
	public static synchronized void saveLastSearch(String viewName, SearchDTO searchDTO) {
		SearchListDTO existingElement = null;

		final var newElement = new SearchListDTO(-1, "", viewName);
		newElement.setUserName(ServiceLocator.getUserName());

		// Check if a last search exists
		final List<SearchListDTO> list = PersistenceHelper.findPersistentObjects(SearchListDTO.class);

		for (final SearchListDTO listEntry : list)
			if (listEntry.equals(newElement)) {
				existingElement = listEntry;
				break;
			}

		if (existingElement == null) {
			final int id = getNextId();
			newElement.setId(id);

			PersistenceHelper.addPersistentObject(newElement);

			searchDTO.setId(id);

			// Create a clone of the given search object in order to save its current state properly!
			PersistenceHelper.addPersistentObject(createClone(searchDTO));
			PersistenceHelper.save();

			return;
		}

		overwriteSavedSearchObject(existingElement.getId(), searchDTO);
	}

	/**
	 * Get the last search
	 * @param viewName the name of the view
	 * @return the search data transfer object
	 */
	public static synchronized SearchDTO getLastSearch(String viewName) {
		// Check if a last search exists
		final List<SearchListDTO> list = PersistenceHelper.findPersistentObjects(SearchListDTO.class);

		for (final SearchListDTO listEntry : list) {
			// Skip invalid entries
			if (listEntry.getUserName() == null || listEntry.getName() == null || listEntry.getViewName() == null)
				continue;

			if (listEntry.getUserName().equals(ServiceLocator.getUserName()) && listEntry.getName().isEmpty()
					&& listEntry.getViewName().equals(viewName))
				return getSavedSearch(listEntry.getId());
		}

		return null;
	}

}
